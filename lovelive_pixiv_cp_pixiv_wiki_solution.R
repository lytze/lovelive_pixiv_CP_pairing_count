require(XML)
require(RCurl)
require(magrittr)

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
    cookiejar = '~/cookies.txt' ,
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

## Get the tags for pairings from pixiv wiki
pairing_keyword_table <- getURL('http://dic.pixiv.net/a/ラブライブ!のカップリングタグ一覧', 
                                curl = curl) %>%
    htmlTreeParse(useInternalNodes = T) %>%
    xpathApply(path = '//section[@id="article-body"]/table[1]', readHTMLTable) %>%
    `[[`(1)
pairing_keyword_table <- as.matrix(pairing_keyword_table)
rownames(pairing_keyword_table) <- pairing_keyword_table[, 1]
pairing_keyword_table <- pairing_keyword_table[, -1]
pairing_keyword_table <- gsub('[◎☆※]', '', pairing_keyword_table)

tags <- c('小泉花陽', '星空凛', '西木野真姫',
          '高坂穂乃果', '南ことり', '園田海未',
          '絢瀬絵里', '矢澤にこ', '東條希')
# 穂乃果	絵里	ことり	海未	凛	真姫	希	花陽	にこ
dimnames(pairing_keyword_table) <- list(tags[c(4, 7, 5, 6, 2, 3, 9, 1, 8)],
                                        tags[c(4, 7, 5, 6, 2, 3, 9, 1, 8)])

## Get fan art number based on pairing tags
score_mat <- matrix(0, 9, 9)
dimnames(score_mat) <- list(tags, tags)
for (i in 2:9) for (j in 1:{i - 1}) {
    cat(tags[i], '---', tags[j], '\n')
    s_1 <- paste('http://www.pixiv.net/search.php?s_mode=s_tag&word=',
                 pairing_keyword_table[tags[i], tags[j]], sep = '') %>%
        getURL(curl = curl) %>%
        htmlTreeParse(useInternalNodes = T) %>%
        xpathSApply('//div[@class="column-label"]/span[@class="count-badge"]', xmlValue) %>%
        {if(is.null(.)) 0 else gsub('[a-z]+', '', .) %>% as.numeric}
    cat(pairing_keyword_table[tags[i], tags[j]], s_1, '\n')
    s_2 <- paste('http://www.pixiv.net/search.php?s_mode=s_tag&word=',
                 pairing_keyword_table[tags[j], tags[i]], sep = '') %>%
        getURL(curl = curl) %>%
        htmlTreeParse(useInternalNodes = T) %>%
        xpathSApply('//div[@class="column-label"]/span[@class="count-badge"]', xmlValue) %>%
        {if(is.null(.)) 0 else gsub('[a-z]+', '', .) %>% as.numeric}
    cat(pairing_keyword_table[tags[j], tags[i]], s_2, '\n')
    score_mat[i, j] <- s_1 + s_2
    score_mat[j, i] <- s_1 + s_2
}

## Save score matrix
write.table(score_mat, 'score_mat_pixiv_wiki_solution.txt')
