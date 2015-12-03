## Ctrip社交网络demo
### 基本说明

- 依赖的R包:
    + shiny
    + networkD3
    + dplyr

    
- main.R
    + 数据处理
    + 基本可视化
    + 保存为Rdata文件,方便shiny调用

- data文件夹
    + 保存了基本数据信息
    + network.Rdata,清理好的数据信息

- Network文件夹
    + ui.R: shiny的UI设计
    + server.R shiny的server设计
    + utils.R 其他需要使用的函数

    
    
### 使用说明

- windows可以在Rstudio里面打开ui.R,并使用RunApp来开启shiny界面
- Rstudio Server打开方式相同(需要预先安装好依赖包)
- windows如果中文显示有问题,尝试:
    + Sys.setlocale("LC_CTYPE","chs")

    
任何问题,联系: langdw@Ctrip.com