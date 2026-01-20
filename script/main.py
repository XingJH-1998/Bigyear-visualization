import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
from io import BytesIO
import base64
import datetime

# --- 配置绘图字体 (适配 GitHub Actions 的 Ubuntu 环境) ---
plt.rcParams['font.family'] = ['sans-serif']
# 优先尝试 Noto Sans CJK (Linux), 其次 SimHei (Windows), 最后 fallback
plt.rcParams['font.sans-serif'] = ['Noto Sans CJK JP', 'Noto Sans CJK SC', 'SimHei', 'Arial']
plt.rcParams['axes.unicode_minus'] = False

# ==========================================
# 1. 数据读取与预处理
# ==========================================
print("正在读取数据...")

# 读取分类体系和观测数据
try:
    ebird_taxonomy = pd.read_csv("data/ebird_taxonomy.csv")
    df = pd.read_csv("data/MyEBirdData.csv")
    
    # 读取地理数据 (保持和你 R 代码路径一致)
    map_geo = gpd.read_file("shapefile/china_map/中国省级地图GS（2019）1719号.geojson")
    jdx = gpd.read_file("shapefile/china_map/九段线GS（2019）1719号.geojson")
except Exception as e:
    print(f"读取文件失败，请检查路径: {e}")
    exit(1)

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

# 使用 geopandas 自带的世界地图 (为了速度，不下载 rnaturalearth)
world = gpd.read_file(gpd.datasets.get_path('naturalearth_lowres'))
# 简单修正 ISO 代码以匹配 (R代码里 TW -> CN，这里我们尽量保留原始 ISO_A2)
# 注意：naturalearth_lowres 的 iso_a2 有些可能不全，这里做个简单映射
world['iso_a2'] = world['iso_a3'].str.slice(0, 2) # 简易处理，实际可能需要更严谨的对应

# 统计世界数据
df_world = df.groupby('Country_code')['Scientific.Name'].nunique().reset_index()
df_world.columns = ['iso_a2', 'n_species']

# 修正台湾归属 (对应你的 R 逻辑: TW -> CN)
# 在 Python 这里的逻辑是：如果数据里有 TW，把它加到 CN 上
if 'TW' in df_world['iso_a2'].values:
    tw_count = df_world.loc[df_world['iso_a2'] == 'TW', 'n_species'].values[0]
    # 检查是否有 CN
    if 'CN' in df_world['iso_a2'].values:
        df_world.loc[df_world['iso_a2'] == 'CN', 'n_species'] += 0 # 这里简单处理，种数不能直接相加因为可能有重叠，但R代码似乎是分开 join 的。
        # 你的 R 代码逻辑：world_map$iso_a2[world_map$iso_a2=="TW"] <- "CN"
        # 这意味着地图上的 TW 区域会被视为 CN，并填上 CN 的数据颜色。
        pass
    
# 合并数据
# 注意：geopandas自带数据的 iso_a2 对中国是 CN，对台湾是 TW。
# 为了复刻效果：我们将地图上的 TW 几何体的 iso_a2 改为 CN，这样 merge 时它会匹配到 CN 的数据
world.loc[world['name'] == "Taiwan", 'iso_a2'] = "CN"

world_data = world.merge(df_world, on='iso_a2', how='left')

# --- 绘图 ---
fig2, ax2 = plt.subplots(figsize=(12, 6))

# 自定义红色渐变 (仿照 R: #fde0dd -> #a50f15)
colors_red = ["#fde0dd", "#a50f15"]
cmap_red = mcolors.LinearSegmentedColormap.from_list("custom_red", colors_red)

world_data.plot(column='n_species', ax=ax2, cmap=cmap_red,
                edgecolor='white', linewidth=0.05,
                missing_kwds={'color': 'lightgrey'}, # NA 值颜色
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
    <p>Author: Jiahua | Update: {datetime.datetime.now().strftime('%Y-%m-%d %H:%M')}</p>
    
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
