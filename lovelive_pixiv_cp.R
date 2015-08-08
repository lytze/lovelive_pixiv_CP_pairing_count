require(XML)
require(RCurl)
require(magrittr)

## Initializing
## ## Idol names' tag
tags <- c('小泉花陽', '星空凛', '西木野真姫',
          '高坂穂乃果', '南ことり', '園田海未',
          '絢瀬絵里', '矢澤にこ', '東條希')
## ## Make a matrix saving linage times
score_mat <- matrix(0, nrow = 9, ncol = 9)
rownames(score_mat) <- tags
colnames(score_mat) <- tags

## Login to Pixiv
header <- c(
    'User-Agent' = paste('Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_4)',
                         ' AppleWebKit/600.7.12 (KHTML, like Gecko) Version/8.0.7 Safari/600.7.12',
                         sep = ''),
    'Accept' = 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
    'Accept-Language' = 'en-us',
    'Connection' = 'keep-alive',
    'Accept-Charset' = 'utf-8;q=0.7,*;q=0.7'
)
options(RCurlOptions = list(
    cainfo = system.file("CurlSSL", "cacert.pem",
                         package = "RCurl")
    )
)
curl <- getCurlHandle()
curlSetOpt(
    cookiejar = 'cookies.txt' ,
    httpheader = header,
    followlocation = TRUE ,
    autoreferer = TRUE ,
    curl = curl
)
params <- c(
    mode = 'login',
    return_to = '/',
    pixiv_id = 'lytze',
    pass = readLines('~/rmysql_pw'),
    skip = '1'
)
tmp <- postForm('https://www.secure.pixiv.net/login.php', .params = params, curl = curl,
                .opt = list(cookiefile = ''), style = 'POST')

## Get the lovelive! tagged works' url
url_works <- character(0)
for (page_n in 1:1000) {
    cat('Getting page:', page_n, 'in 1000\n')
    url <- paste('http://www.pixiv.net/search.php',
                 '?word=ラブライブ!&type=illust&order=date_d&p=',
                 page_n, sep = '')
    cat(url, '\n')
    document <- htmlTreeParse(getURL(url, curl = curl), useInternalNodes = T)
    works <- xpathSApply(document, '//li[@class="image-item "]/*[2]', xmlAttrs)
    url_works <- union(url_works, paste('http://www.pixiv.net', works, sep = ''))
    cat(length(url_works), '\n')
}
## Clean the urls to rule out duplicates (due to newly coming works)
url_works <- url_works[!duplicated(url_works)]
## Get tags for each work
i <- 1
works_num <- length(url_works)
for (url_work in url_works) {
    cat('Getting taggs:', i, 'in', works_num, '\n')
    i <- i + 1
    tags_used <- getURL(url_work, curl = curl) %>%
        htmlTreeParse(useInternalNodes = T) %>%
        xpathSApply('//li[@class="tag"]/a[@class="text"]', xmlValue) %>%
        {.[. %in% tags]}
    if (length(tags_used) >= 2 && length(tags_used) < 6) {
        ## When there are over 2 idol name tag and
        ## the picture is obviously not a mu's all-in-one
        ## add score +1 to all combinations
        tags_used %>%
            combn(m = 2) %>%
            apply(2, function(pair) {
                print(pair)
                score_mat[pair[1], pair[2]] <<- score_mat[pair[1], pair[2]] + 1
                score_mat[pair[2], pair[1]] <<- score_mat[pair[2], pair[1]] + 1
            })
        write.table(score_mat, '~/Desktop/score_mat.txt')
    }
}