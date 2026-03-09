#!/bin/bash

# 1. 切换到你的项目目录
cd /Users/tangerine/Documents/Big_year || exit

# 2. 打印一些好看的提示信息
echo "========================================"
echo "🐦 正在启动自动同步监控..."
echo "📂 工作目录: $(pwd)"
echo "📡 状态: 等待文件保存..."
echo "========================================"

# 3. 执行监控命令
# 注意：这里我加了一个简单的错误处理，如果 git 报错不会直接关掉窗口
fswatch -o /Users/tangerine/Documents/Big_year/data/MyEBirdData.csv | xargs -n1 -I{} sh -c 'git add data/MyEBirdData.csv && git commit -m "Auto update csv" && git push && date "+%H:%M:%S Uploaded"'