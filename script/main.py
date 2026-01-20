import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
from io import BytesIO
import base64
import datetime
import platform

# ==========================================
# 0. 字体配置 (解决中文乱码核心代码)
# ==========================================
system_name = platform.system()

if system_name == "Darwin":  # macOS (你的本地电脑)
    # macOS 优先使用系统自带的 "Arial Unicode MS" 或 "PingFang SC"
    plt.rcParams['font.sans-serif'] = ['Arial Unicode MS', 'PingFang SC', 'Heiti TC', 'sans-serif']
elif system_name == "Linux": # GitHub Actions (服务器)
    # Ubuntu 服务器优先使用 Noto Sans CJK
    plt.rcParams['font.sans-serif'] = ['Noto Sans CJK JP', 'Noto Sans CJK SC', 'WenQuanYi Micro Hei', 'sans-serif']
else: # Windows
    plt.rcParams['font.sans-serif'] = ['SimHei', 'Microsoft YaHei', 'sans-serif']

# 解决负号显示为方块的问题
plt.rcParams['axes.unicode_minus'] = False

# ==========================================
# 1. 数据读取与预处理
# ==========================================
print("正在读取数据...")

try:
    ebird_taxonomy = pd.read_csv("data/ebird_taxonomy.csv")
    df = pd.read_csv("data/MyEBirdData.csv")

    # 【新增代码】模拟 R 的列名处理：把空格和斜杠变成点
    df.columns = df.columns.str.replace(' ', '.').str.replace('/', '.')
    
    # 打印一下列名，确保改对了（调试用）
    print("Columns:", df.columns.tolist()) 

    # 读取地理数据
    map_geo = gpd.read_file("shapefile/china_map/中国省级地图GS（2019）1719号.geojson")
    jdx = gpd.read_file("shapefile/china_map/九段线GS（2019）1719号.geojson")
except Exception as e:
    print(f"读取文件失败，请检查路径: {e}")
    exit(1)

# 下面继续原本的代码...
# 处理日期和筛选年份
df['Date'] = pd.to_datetime(df['Date'])
df['Year'] = df['Date'].dt.year
df = df[df['Year'] == 2026].copy()

# 修正科学名 (Left Join logic)
df = pd.merge(df, ebird_taxonomy, left_on="Scientific.Name", right_on="SCI_NAME", how="left")

# 逻辑：如果 REPORT_AS 匹配到了 SPECIES_CODE，则用 SCI_NAME 修正 Scientific.Name
# 为了复刻 R 的逻辑，我们需要先建立一个映射字典
code_to_sciname = dict(zip(ebird_taxonomy['SPECIES_CODE'], ebird_taxonomy['SCI_NAME']))

def correct_name(row):
    report_as = row['REPORT_AS']
    if pd.notna(report_as) and report_as in code_to_sciname:
        return code_to_sciname[report_as]
    return row['Scientific.Name']

df['Scientific.Name'] = df.apply(correct_name, axis=1)

# 筛选类别
valid_categories = ["domestic", "issf", "species"]
df = df[df['CATEGORY'].isin(valid_categories)]

# 处理省份和地区代码
df['Country_code'] = df['State.Province'].str.slice(0, 2)

def clean_province(prov):
    if str(prov).startswith("HK-"): return "HK"
    if str(prov).startswith("TW-"): return "TW"
    if str(prov).startswith("MO-"): return "MO"
    return prov

df['State.Province'] = df['State.Province'].apply(clean_province)

# 省份映射字典
prov_dict = {
  "CN-11": "北京市","CN-12": "天津市","CN-13": "河北省","CN-14": "山西省","CN-15": "内蒙古自治区",
  "CN-21": "辽宁省","CN-22": "吉林省","CN-23": "黑龙江省","CN-31": "上海市","CN-32": "江苏省",
  "CN-33": "浙江省","CN-34": "安徽省","CN-35": "福建省","CN-36": "江西省","CN-37": "山东省",
  "CN-41": "河南省","CN-42": "湖北省","CN-43": "湖南省","CN-44": "广东省","CN-45": "广西壮族自治区",
  "CN-46": "海南省","CN-50": "重庆市","CN-51": "四川省","CN-52": "贵州省","CN-53": "云南省",
  "CN-54": "西藏自治区","CN-61": "陕西省","CN-62": "甘肃省","CN-63": "青海省","CN-64": "宁夏回族自治区",
  "CN-65": "新疆维吾尔自治区",
  "TW": "台湾省","HK": "香港特别行政区","MO": "澳门特别行政区"
}

# ==========================================
# 2. 中国地图处理
# ==========================================
print("正在生成中国地图...")

df_china = df[df['State.Province'].isin(prov_dict.keys())].copy()
df_china['CNAME'] = df_china['State.Province'].map(prov_dict)

# 统计每个省的种数
china_summary = df_china.groupby('CNAME')['Scientific.Name'].nunique().reset_index()
china_summary.columns = ['CNAME', 'n_species']

# 合并地图数据
map_data = map_geo.merge(china_summary, on='CNAME', how='left')
map_data['n_species'] = map_data['n_species'].fillna(0)

# --- 绘图 (Matplotlib) ---
fig1, ax1 = plt.subplots(figsize=(10, 8))

# 绘制九段线
jdx.plot(ax=ax1, linewidth=0.5, color="black", zorder=2)

# 绘制省份
# 自定义 colormap (仿照 R 代码: white -> lightblue -> royalblue)
colors = ["#FFFFFF", "#ADD8E6", "#4169E1"]
cmap = mcolors.LinearSegmentedColormap.from_list("custom_blue", colors)

map_data.plot(column='n_species', ax=ax1, cmap=cmap, 
              edgecolor='grey', linewidth=0.3, legend=True,
              legend_kwds={'label': "鸟种数", 'orientation': "horizontal", 'shrink': 0.6})

# 添加标签
for idx, row in map_data.iterrows():
    # 只在数量大于0时显示标签，或者你可以全部显示
    if row['n_species'] > 0:
        # 获取几何中心用于放置文字
        centroid = row.geometry.centroid
        ax1.text(centroid.x, centroid.y, str(int(row['n_species'])), 
                 fontsize=8, ha='center', va='center', color='black')

ax1.set_title("中国鸟种数", fontsize=16, fontweight='bold')
ax1.axis('off')

# 将图片保存为 Base64 字符串
buf1 = BytesIO()
plt.savefig(buf1, format='png', bbox_inches='tight', dpi=150)
buf1.seek(0)
img1_base64 = base64.b64encode(buf1.read()).decode('utf-8')
plt.close(fig1)

# ==========================================
# 3. 世界地图处理
# ==========================================
print("正在生成世界地图...")

try:
    world = gpd.read_file("shapefile/world_map.zip")
except Exception as e:
    print(f"❌ 无法读取地图: {e}")
    exit(1)

world.columns = world.columns.str.lower()

# -----------------------------------------------------------
# 第一步：修改【数据】(Data)
# 逻辑：在统计之前，把 TW, HK, MO 的代码全部改成 CN
# 这样 groupby 统计时，会自动把这些地区的鸟种合并，且去除重复
# -----------------------------------------------------------
df_for_world = df.copy() # 创建副本，不影响前面的中国分省地图

# 将 台湾(TW), 香港(HK), 澳门(MO) 的代码全部统一为 CN
target_areas = ['TW', 'HK', 'MO']
df_for_world.loc[df_for_world['Country_code'].isin(target_areas), 'Country_code'] = 'CN'

# 现在统计世界数据，CN 就会包含大陆+港澳台的所有去重鸟种
df_world = df_for_world.groupby('Country_code')['Scientific.Name'].nunique().reset_index()
df_world.columns = ['iso_a2', 'n_species']

#print("CN (含港澳台) 总种数:", df_world[df_world['iso_a2'] == 'CN']['n_species'].values)


# -----------------------------------------------------------
# 第二步：修改【地图】(Map)
# 逻辑：让地图上的 CHN(大陆), TWN(台湾) 都去匹配 CN 这个数据
# -----------------------------------------------------------

# 1. 优先使用 adm0_a3 (三位代码) 来确定国家
if 'iso_a2' not in world.columns:
    world['iso_a2'] = ''

# 2. 建立映射关系：把地图上的哪些块，视为 "CN"
# 这里的 Key 是地图自带的3位代码，Value 是你要匹配的数据代码
iso_mapping = {
    'CHN': 'CN',  # 中国大陆 -> 读 CN 的数据
    'TWN': 'CN',  # 台湾 -> 读 CN 的数据 (关键修改！)
    'HKG': 'CN',  # 香港 -> 读 CN 的数据
    'MAC': 'CN'   # 澳门 -> 读 CN 的数据
}

# 3. 应用映射
for iso3, iso2 in iso_mapping.items():
    world.loc[world['adm0_a3'] == iso3, 'iso_a2'] = iso2

# 4. 其他国家正常处理 (防瑞士陷阱)
mask = ~world['adm0_a3'].isin(iso_mapping.keys())
# 仅对未被手动修正的行进行默认截取
world.loc[mask, 'iso_a2'] = world.loc[mask, 'adm0_a3'].str.slice(0, 2)


# -----------------------------------------------------------
# 合并与绘图
# -----------------------------------------------------------
world_data = world.merge(df_world, on='iso_a2', how='left')

fig2, ax2 = plt.subplots(figsize=(12, 6))

colors_red = ["#fde0dd", "#a50f15"]
cmap_red = mcolors.LinearSegmentedColormap.from_list("custom_red", colors_red)

world_data.plot(column='n_species', ax=ax2, cmap=cmap_red,
                edgecolor='white', linewidth=0.05,
                missing_kwds={'color': 'lightgrey'},
                legend=True,
                legend_kwds={'label': "鸟种数", 'orientation': "horizontal", 'shrink': 0.5})

ax2.set_title("世界鸟种数", fontsize=16, fontweight='bold')
ax2.set_axis_off()

buf2 = BytesIO()
plt.savefig(buf2, format='png', bbox_inches='tight', dpi=150)
buf2.seek(0)
img2_base64 = base64.b64encode(buf2.read()).decode('utf-8')
plt.close(fig2)

# ==========================================
# 4. 生成 HTML 文件
# ==========================================
html_content = f"""
<!DOCTYPE html>
<html>
<head>
    <meta charset="utf-8">
    <title>2026大年鸟种数</title>
    <style>
        body {{ font-family: "Helvetica Neue", Helvetica, Arial, sans-serif; text-align: center; padding: 20px; }}
        h1 {{ color: #333; }}
        .plot-container {{ margin-bottom: 50px; }}
        img {{ max-width: 100%; height: auto; box-shadow: 0 4px 8px rgba(0,0,0,0.1); }}
        .footer {{ color: #777; font-size: 0.8em; margin-top: 50px; }}
    </style>
</head>
<body>
    <h1>2026大年鸟种数</h1>
    <p>Author: Jiahua | Auto update: {datetime.datetime.now().strftime('%Y-%m-%d %H:%M')}</p>
    
    <div class="plot-container">
        <img src="data:image/png;base64,{img1_base64}" alt="China Map">
    </div>
    
    <div class="plot-container">
        <img src="data:image/png;base64,{img2_base64}" alt="World Map">
    </div>

    <div class="footer">
        Generated by Python & GitHub Actions
    </div>
</body>
</html>
"""

with open("index.html", "w", encoding="utf-8") as f:
    f.write(html_content)

print("生成完成！index.html 已更新。")
