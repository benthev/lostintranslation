library(jsonlite)
library(translateR)
library(utils)

getGoogleLanguages()



#key <- PUTYOURAPIKEYHERE


google_translate <- function(translate_this, source_lang, target_lang, key) {
  base_url <- 'https://www.googleapis.com/language/translate/v2'
  query <- URLencode(paste0(base_url, '?key=', key, '&q=', translate_this, '&source=', source_lang, '&target=', target_lang))
  result <- fromJSON(query)
  text <- unlist(result)
  return(text)
}
translate_this <- 'Greetings citizen! how are you doing this fine day?'
google_translate(translate_this = translate_this, source_lang = 'en', target_lang = 'zh-CN', key = key)
google_translate(translate_this = google_translate(translate_this = translate_this, source_lang = 'en', target_lang = 'zh-CN', key = key), 
                 source_lang = 'zh-CN', target_lang = 'en', key = key)

lost_in_translation <- function(original_text, iterations, source_lang, target_lang, key) {
  translate_this <- original_text
  for (i in 1:iterations) {
    translation <- google_translate(translate_this = translate_this, source_lang = source_lang, target_lang = target_lang, key = key)
    retranslation <- google_translate(translate_this = translation, source_lang = target_lang, target_lang = source_lang, key = key)
    #Stopping mechanism w/ convergence
    if(retranslation == translate_this) {
      print(paste("Convergence after", i, "iterations"))
      return(retranslation)
    } else {
      translate_this <- retranslation
    }
  }
  final_text <- translate_this
  return(final_text)
}

lost_in_translation(original_text = translate_this, iterations = 5, source_lang = 'en', target_lang = 'zh-CN', key = key)



lost_in_translation(original_text = 'emotion recollected in tranquility', iterations = 50, source_lang = 'en', target_lang = 'zh-CN', key = key)



