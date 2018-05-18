# bgm_as_sf <- function(x) {
#   d <- tibble::tibble(name = c(x$face$face, x$box$box, "boundary"))
#   d[["geometry"]] <- sf::st_sfc(c(make_faces(x$face), make_boxes(x$verts), list(make_boundary(x$verts))))
#   
#   sf::st_as_sf(d)
# }
# make_boundary <- function(tab) {
#   tab <- tab[grepl("bnd", tab$vert), ]
#   sf::st_linestring(as.matrix(tab[, c("X", "Y")]))
# }
# make_boxes <- function(tab) {
#   tab <- tab[grepl("box", tab$vert), ]
#   lapply(split(tab[, c("X", "Y")], tab$vert), function(x) sf::st_polygon(list(as.matrix(x))))
# }
# make_faces <- function(tab) {
#   lapply(split(tab[, c("x1", "y1", "x2", "y2")], tab$face), function(x) sf::st_linestring(matrix(unlist(x), ncol = 2, byrow = TRUE)))
# }
# 
# read_bgm <- function(x) {
#   if (nchar(x) < 1) stop("file path is empty string")
#   if (!file.exists(x)) mess <- stop(sprintf("no file found '%s'\n", x))
#   #system.time(tx <- readLines(x)) 
#   tx <- unlist(strsplit(readr::read_file(x), "\n"))
#   nch <- length(tx)
#   if (nch == 0) {
#     stop(sprintf("no lines found in file %s", x))
#   }
#   tx <- gsub("\\t", " ", tx)
#   list(face = faces_parse(tx), 
#        box = boxes_parse(tx), 
#        vertex = verts_parse(tx), 
#        iface = iface_parse(tx), 
#        meta = meta_parse(tx) %>% dplyr::mutate(projection = fixproj(projection)))
# }
# meta_parse <- function(tx) {
#   tags <- c("maxwcbotz", "nbox", "nface", "projection")
#   tibble::as_tibble(lapply(stats::setNames(lapply(tags, 
#              function(a) gsub("^\\s+", "", gsub(a, "", grep(a, tx, value = TRUE)))), 
#              tags), type.convert, as.is = TRUE))
# }
# iface_parse <- function(tx) {
#   bind_rows(lapply(strsplit(stringr::str_subset(tx, "box[0-9]{1,}.iface"), " "), function(x) tibble::tibble(name = x[1], iface = as.integer(x[-1])))) %>% 
#     mutate(name = gsub("\\.iface", "", .data$name))
# }
# verts_parse <- function(tx) {
#   read_delim_pattern(tx, "vert ", col_names = c("vert", "X", "Y")) %>% mutate(vert = gsub("\\.vert", "", vert))
# }
# boxes_parse <- function(tx) {
#   label <- read_delim_pattern(tx, "box[0-9]{1,}\\.label", col_names = c("box", "label")) %>% mutate(box = gsub("\\.label", "", .data$box))
#   inside <- read_delim_pattern(tx, "box[0-9]{1,}\\.inside", col_names = c("box", "insideX", "insideY"))
#   nconn <- read_delim_pattern(tx, "box[0-9]{1,}\\.nconn", col_names = c("box", "nconn"))
#   
# # iface <- read_delim_pattern(tx, "box[0-9]{1,}\\.iface)
# # ibox
#   botz <- read_delim_pattern(tx, "box[0-9]{1,}\\.botz", col_names = c("box", "botz"))
#   area <- read_delim_pattern(tx, "box[0-9]{1,}\\.area", col_names = c("box", "area"))
#   vertmix <- read_delim_pattern(tx, "box[0-9]{1,}\\.vertmix", col_names = c("box", "vertmix"))
#   horizmix <- read_delim_pattern(tx, "box[0-9]{1,}\\.horizmix", col_names = c("box", "horizmix"))
#   
#   bind_cols(label, inside[,-1], nconn[, -1], botz[, -1], area[, -1], vertmix[, -1], horizmix[, -1])
# }
# 
# faces_parse <- function(tx) {
#   p1 <- read_delim_pattern(tx, "face[0-9]{1,}\\.p1", col_names = c("face", "x1", "y1")) %>% dplyr::mutate(face = gsub("\\.p1", "", .data$face))
#   p2 <- read_delim_pattern(tx, "face[0-9]{1,}\\.p2", col_names = c("face", "x2", "y2"))
#   len <- read_delim_pattern(tx, "face[0-9]{1,}\\.length", col_names = c("face", "length"))
#   cs <- read_delim_pattern(tx, "face[0-9]{1,}\\.cs", col_names = c("face", "cosine", "sine"))
#   lr <- read_delim_pattern(tx, "face[0-9]{1,}\\.lr", col_names = c("face", "left", "right"))
#   bind_cols(p1, p2[, -1], len[, -1], cs[, -1], lr[, -1])
# }
# read_delim_pattern <- function(x, pattern, col_names = FALSE) {
#   readr::read_delim(paste(stringr::str_subset(x, pattern), collapse = "\n"), delim = " ", col_names = col_names)
# }