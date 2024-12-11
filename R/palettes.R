#' Color palettes and information
#'
#' @keywords internal
discrete_palettes <- list(
  tidyplots.apple = c("#ff3b30","#ff9500","#ffcc00","#4cd964","#5ac8fa","#007aff","#5856d6"),
  tidyplots.candy = c("#9b5de5","#f15bb5","#fee440","#00bbf9","#00f5d4"),
  tidyplots.friendly = c("#0072B2","#56B4E9","#009E73","#F5C710","#E69F00","#D55E00"),
  tidyplots.friendly_long = c("#CC79A7","#0072B2","#56B4E9","#009E73","#F5C710","#E69F00","#D55E00"),
  tidyplots.ibm = c("#5B8DFE","#725DEE","#DD227D","#FE5F00","#FFB109"),
  tidyplots.metro = c("#4DACD6","#4FAE62","#F6C54D","#E37D46","#C02D45"),
  tidyplots.okabeito = c("#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7"),
  tidyplots.seaside = c("#8ecae6","#219ebc","#023047","#ffb703","#fb8500"),
  nature.cyclone = c("#D75425","#E6b532","#7E2f8C","#52BCEC","#A21D2F","#73AA43","#2B2A76"),
  sxs.vintage  = c("#5D8CC2", "#F1BC3A", "#E7673A", "#50A07D", "#0D294E"),
  muke.collapse = c("#13366B","#043D45","#003D74","#0796B5","#4DA280","#406C94","#3E441C","#CC5D20","#E1C199","#FED0DA","#FE5E10","#F8C6B5","#CB9746","#E84F8C"),
  hdz.public = c("#003153","#002FA7","#008C8C","#81D8D0","#F9DC24","#E85827","#B05923","#8F4B28","#800020","#4C0009"),
  akun.ukiyo = c("#E86254","#EF8A46","#F8AA59","#FFD06E","#FEE6B5","#ABDCE0","#73BCD5","#528FAC","#386795","#1F466F")
)

#' @keywords internal
continuous_palettes <- list(
  tidyplots.magma = c("#000004FF","#02020DFF","#060519FF","#0C0927FF","#130D34FF","#1C1044FF","#231252FF","#2E1162FF","#38106CFF","#420F75FF","#4E117BFF","#57157EFF","#611880FF","#6A1C81FF","#752181FF","#7D2482FF","#882781FF","#922B81FF","#9B2E7FFF","#A6317DFF","#AF357BFF","#BB3978FF","#C43C75FF","#CF4070FF","#D8456CFF","#DF4B68FF","#E85362FF","#EE5B5EFF","#F4675CFF","#F7715CFF","#FA7E5EFF","#FC8961FF","#FD9668FF","#FEA16EFF","#FEAB75FF","#FEB87EFF","#FEC287FF","#FECF92FF","#FDD99BFF","#FDE6A8FF","#FCF0B2FF","#FCFDBFFF"),
  tidyplots.cividis = c("#00204DFF","#002557FF","#002961FF","#002D6DFF","#00306FFF","#01366EFF","#173A6DFF","#253E6CFF","#2E436CFF","#36476BFF","#3F4C6BFF","#45506BFF","#4C546CFF","#52596CFF","#595E6DFF","#5E626EFF","#64666FFF","#696B71FF","#6E6F73FF","#747475FF","#797977FF","#7F7D78FF","#848279FF","#8B8779FF","#918C78FF","#979178FF","#9D9677FF","#A39A76FF","#ABA074FF","#B1A573FF","#B7AA71FF","#BEAF6FFF","#C5B56CFF","#CCBB69FF","#D2C066FF","#D9C562FF","#E0CB5EFF","#E8D259FF","#EED753FF","#F7DD4DFF","#FDE346FF","#FFEA46FF"),
  tidyplots.inferno = c("#000004FF","#02020EFF","#07051AFF","#0D082AFF","#150B37FF","#1F0C48FF","#290B54FF","#350A60FF","#3E0966FF","#490B6AFF","#540F6DFF","#5D126EFF","#68166EFF","#71196EFF","#7C1D6DFF","#85216BFF","#8F2568FF","#992766FF","#A22B62FF","#AD305DFF","#B53458FF","#C03952FF","#C73E4CFF","#D04545FF","#D84C3EFF","#DF5237FF","#E55C30FF","#EB6429FF","#F06F20FF","#F47918FF","#F7840FFF","#F98E09FF","#FB9B06FF","#FCA60CFF","#FCB115FF","#FBBF24FF","#F9C932FF","#F6D746FF","#F3E259FF","#F1EE73FF","#F3F68BFF","#FCFFA4FF"),
  tidyplots.mako = c("#0B0405FF","#12080DFF","#180D16FF","#1E111FFF","#241628FF","#2A1B33FF","#2F1F3DFF","#342547FF","#372852FF","#3B2D5BFF","#3E3367FF","#403871FF","#413E7EFF","#414387FF","#3F4B90FF","#3D5296FF","#3A5A9AFF","#38619DFF","#37689FFF","#3670A0FF","#3576A2FF","#357DA3FF","#3484A5FF","#348CA7FF","#3492A8FF","#3499AAFF","#35A0ABFF","#37A6ACFF","#3AAEADFF","#3FB5ADFF","#44BDADFF","#4BC2ADFF","#55CAADFF","#61CFACFF","#72D4ADFF","#86D9B1FF","#96DDB5FF","#A9E1BDFF","#B6E6C5FF","#C5EAD0FF","#D0EFDAFF","#DEF5E5FF"),
  tidyplots.plasma = c("#0D0887FF","#1C068EFF","#290593FF","#360498FF","#3F049CFF","#4B03A1FF","#5502A4FF","#5F01A6FF","#6700A8FF","#7100A8FF","#7B02A8FF","#8405A7FF","#8E0BA5FF","#9511A1FF","#9E199DFF","#A62098FF","#AD2792FF","#B42E8DFF","#BB3488FF","#C23C81FF","#C8437BFF","#CD4A76FF","#D35171FF","#D9586AFF","#DE5F65FF","#E26560FF","#E76E5BFF","#EB7556FF","#EF7E50FF","#F3854BFF","#F68E44FF","#F89540FF","#FB9F3AFF","#FCA835FF","#FDB030FF","#FEBA2CFF","#FDC328FF","#FCCE25FF","#FAD824FF","#F7E325FF","#F4ED27FF","#F0F921FF"),
  tidyplots.rocket = c("#03051AFF","#0A091FFF","#120D25FF","#1C112BFF","#241432FF","#2E1739FF","#36193FFF","#411B44FF","#491D49FF","#531E4DFF","#5E1F52FF","#671F55FF","#721F57FF","#7B1F59FF","#871E5BFF","#921C5BFF","#9D1B5BFF","#A7195AFF","#B01759FF","#BC1656FF","#C51852FF","#CE1D4EFF","#D62449FF","#DE2E44FF","#E43841FF","#E8413EFF","#ED4F3EFF","#EF5A41FF","#F26747FF","#F3724EFF","#F47E57FF","#F58860FF","#F5946BFF","#F69D75FF","#F6A77FFF","#F6B28CFF","#F6BB97FF","#F7C6A6FF","#F7CEB2FF","#F8D8C1FF","#F9E0CEFF","#FAEBDDFF"),
  tidyplots.turbo = c("#30123BFF","#372365FF","#3D3489FF","#4147AEFF","#4456C8FF","#4669E0FF","#4777EFFF","#4688FBFF","#4197FFFF","#38A5FBFF","#2CB7F0FF","#22C4E3FF","#1AD3D1FF","#18DDC2FF","#1DE7B2FF","#29EFA2FF","#3DF58CFF","#53FA79FF","#69FD66FF","#83FF51FF","#98FE43FF","#AAFB39FF","#BAF635FF","#CBED34FF","#D9E436FF","#E4DA38FF","#F0CC3AFF","#F7C13AFF","#FCB136FF","#FEA230FF","#FE8F28FF","#FB7D21FF","#F56918FF","#EF5911FF","#E74A0CFF","#DD3C08FF","#D23105FF","#C32503FF","#B51C01FF","#A31301FF","#910B01FF","#7A0403FF"),
  tidyplots.viridis = c("#440154FF","#460A5DFF","#471264FF","#481B6DFF","#482374FF","#472C7AFF","#46337FFF","#443A83FF","#424186FF","#3F4889FF","#3C508BFF","#39568CFF","#365D8DFF","#33638DFF","#306A8EFF","#2D708EFF","#2B758EFF","#297B8EFF","#26818EFF","#24878EFF","#228D8DFF","#20928CFF","#1F988BFF","#1F9F88FF","#20A486FF","#24AA83FF","#29AF7FFF","#31B57BFF","#3BBB75FF","#45C06FFF","#53C569FF","#5EC962FF","#6ECE58FF","#7BD250FF","#8AD647FF","#9CD93CFF","#AADC32FF","#BDDF26FF","#CCE11EFF","#DEE318FF","#EDE51BFF","#FDE725FF"),
  akun.ukiyo = c("#E86254","#EF8A46","#F8AA59","#FFD06E","#FEE6B5","#ABDCE0","#73BCD5","#528FAC","#386795","#1F466F"),
  xhs.sunset = c("#4D6097","#5F6FA7","#767FB5","#9594C4","#B7A5CD","#DAB4CC","#F0C5C3","#FBCEA0","#F1AE89","#F09771","#E3825C","#D46C49","#C55635","#B44021","#AF3D21")
)

#' @keywords internal
diverging_palettes <- list(
  tidyplots.blue2brown = c("#1961A5","#2671B5","#2D80BF","#268CC9","#119DD8","#00B2EB","#66C5EF","#C4E5F8","#FEFCF6","#FDEEB8","#FCDD67","#F6C445","#E78B43","#DD5642","#DB3E34","#CA3632","#B3322E"),
  tidyplots.blue2red = c("#0000FF","#1F1FFF","#3F3FFF","#5F5FFF","#7F7FFF","#9F9FFF","#BFBFFF","#DFDFFF","#FFFFFF","#FFDFDF","#FFBFBF","#FF9F9F","#FF7F7F","#FF5F5F","#FF3F3F","#FF1F1F","#FF0000"),
  tidyplots.BuRd = c("#053061","#2166AC","#4393C3","#92C5DE","#D1E5F0","#F7F7F7","#FDDBC7","#F4A582","#D6604D","#B2182B","#67001F"),
  tidyplots.BuYlRd = c("#313695","#4575B4","#74ADD1","#ABD9E9","#E0F3F8","#FFFFBF","#FEE090","#FDAE61","#F46D43","#D73027","#A50026"),
  tidyplots.icefire = c("#b7e3d9","#a9d9d6","#98cdd2","#8ac4d0","#72b6ce","#63adcd","#55a3cd","#4394ce","#3987cf","#377cd0","#3f69c9","#465ebe","#4954b0","#474792","#42407b","#3c3a69","#323050","#2c2b42","#272636","#212028","#1f1e21","#201e1e","#261e1f","#332023","#3d2228","#4a252e","#5c2935","#6d2b3b","#7b2d40","#932e44","#a22f44","#b33341","#c53c3c","#d24737","#da5334","#e66734","#eb753a","#ef8445","#f39a5f","#f7ab75","#fab887","#fecea5"),
  tidyplots.spectral = c("#5b53a4","#525fa9","#486cb0","#3f77b5","#3389bd","#3d95b8","#47a0b3","#58b2ac","#64c0a6","#71c6a5","#86cfa5","#94d4a4","#a2d9a4","#b5e1a2","#c3e79f","#cfec9d","#e1f399","#e9f69d","#eef8a4","#f6fbb0","#fcfeba","#fffdbc","#fff7b2","#feeda1","#fee797","#fee08b","#fed27f","#fdc776","#fdbd6d","#fdad60","#fba05b","#f99153","#f67f4b","#f47044","#ef6645","#e55749","#df4e4b","#d9444d","#cb334d","#be254a","#b41947","#a20643"),
  akun.ukiyo = c("#E86254","#EF8A46","#F8AA59","#FFD06E","#FEE6B5","#ABDCE0","#73BCD5","#528FAC","#386795","#1F466F"),
  xhs.sunset = c("#4D6097","#5F6FA7","#767FB5","#9594C4","#B7A5CD","#DAB4CC","#F0C5C3","#FBCEA0","#F1AE89","#F09771","#E3825C","#D46C49","#C55635","#B44021","#AF3D21")
)

#' Color palette information
#'
#' A dataset containing information about all available color palettes in the package
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{palette_name}{Full name of the palette (source.name)}
#'   \item{source}{Source/origin of the palette}
#'   \item{name}{Name of the palette}
#'   \item{type}{Type of the palette: discrete, continuous, or diverging}
#'   \item{n_colors}{Number of colors in the palette}
#'   \item{colors}{List column containing the color hex codes}
#' }
#' @export
palette_info <- data.frame(
  palette_name = c(
    names(discrete_palettes),
    names(continuous_palettes),
    names(diverging_palettes)
  )
)

# Add source and name columns by splitting the palette_name
palette_info$source <- sapply(strsplit(palette_info$palette_name, "\\."), `[`, 1)
palette_info$name <- sapply(strsplit(palette_info$palette_name, "\\."), `[`, 2)
palette_info$type <- c(
  rep("discrete", length(discrete_palettes)),
  rep("continuous", length(continuous_palettes)),
  rep("diverging", length(diverging_palettes))
)
palette_info$n_colors <- c(
  sapply(discrete_palettes, length),
  sapply(continuous_palettes, length),
  sapply(diverging_palettes, length)
)
palette_info$colors <- c(
  discrete_palettes,
  continuous_palettes,
  diverging_palettes
)
