#Specify and load libraries
librariesToLoad <- c('jsonlite', 'googleLanguageR', 'utils', 'xml2')


sapply(librariesToLoad, function(package) {
  if(require(package, character.only=TRUE)){     
    print(paste(package, "loaded correctly", sep=" "))
  } else {
    print(paste("Installing", package, sep=" "))
    install.packages(package)
    if(require(package, character.only=TRUE)){
      print(paste(package, "loaded correctly", sep=" "))
    } else {
      stop(paste("Could not install", package, sep=" "))
    }
  }
})



search()




#Specify unique API Key
#key <- PUTYOURAPIKEYHERE

if (!exists("key")) {
  print('INSERT YOUR API KEY ABOVE')
  print('INSERT YOUR API KEY ABOVE')
  print('INSERT YOUR API KEY ABOVE')
  print('INSERT YOUR API KEY ABOVE')
  print('INSERT YOUR API KEY ABOVE')
} 


#Specify main translation paramaters
#What to translate
translate_this <- 'Greetings citizen! how are you doing this fine day?'
#Default source & target languages
default_source_language <- 'en'
default_target_language <- 'zh-CN'


#Print language codes
fromJSON(URLencode(paste0("https://translation.googleapis.com/language/translate/v2/languages", 
                          "?key=", key)))



#Create simple translation function
google_translate <- function(translate_this, source_lang = 'en', target_lang = 'zh-CN', key = key) {
  base_url <- 'https://www.googleapis.com/language/translate/v2'
  translate_this <- xml_text(read_html(charToRaw(translate_this)))
  query <- URLencode(paste0(base_url, '?key=', key, '&q=', translate_this, '&source=', source_lang, '&target=', target_lang))
  result <- fromJSON(query)
  text <- unlist(result)
  return(text)
}

#Test translate
translate_this
google_translate(translate_this = translate_this, source_lang = 'en', target_lang = 'zh-CN', key = key)
google_translate(translate_this = google_translate(translate_this = translate_this, source_lang = 'en', target_lang = 'zh-CN', key = key), 
                 source_lang = 'zh-CN', target_lang = 'en', key = key)

#Create function for iterative translations
lost_in_translation <- function(original_text, iterations, source_lang, target_lang, key, print_intermediates = FALSE) {
  translate_this <- original_text
  all_translations <- rep(NA, iterations)
  all_retranslations <- rep(NA, iterations)
  for (i in 1:iterations) {
    translation <- google_translate(translate_this = translate_this, source_lang = source_lang, target_lang = target_lang, key = key)
    all_translations[i] <- translation
    retranslation <- google_translate(translate_this = translation, source_lang = target_lang, target_lang = source_lang, key = key)
    all_retranslations[i] <- retranslation
    if(print_intermediates == TRUE) {
      print(retranslation)
    }
    #Stopping mechanism w/ convergence
    if(retranslation == translate_this) {
      print(paste("Convergence after", i-1, "iterations"))
      return(retranslation)
    } else  {
      if(i > 1) {
        if(retranslation %in% all_retranslations[1:(i-1)]) {
          #Cyclic convergence (?)
          print(paste("Trapped in cyclic convergence after", which(retranslation == all_retranslations[1:(i-1)]), "iterations"))
          return(retranslation)
        }
        
      }
      translate_this <- retranslation
    }
  }
  final_text <- translate_this
  return(final_text)
}





#Execute main function
translate_this
default_source_language
default_target_language
lost_in_translation(original_text = translate_this, iterations = 5, source_lang = default_source_language, target_lang = default_target_language, key = key)


#Additional runs
lost_in_translation(original_text = 'emotion recollected in tranquility', iterations = 100, source_lang = default_source_language, target_lang = default_target_language, key = key, print_intermediates = TRUE)


lost_in_translation(original_text = 'need air-conditioning lest leisure take its toll', iterations = 100, source_lang = default_source_language, target_lang = 'ta', key = key)


lost_in_translation(original_text = 'angelheaded hipsters burning for the ancient heavenly connection to the starry dynamo in the machinery of night', iterations = 100, source_lang = default_source_language, target_lang = 'ta', key = key, print_intermediates = TRUE)

lost_in_translation(original_text = "Somebody once told me the world is gonna roll me. I ain't the sharpest tool in the shed. 
She was looking kind of dumb with her finger and her thumb In the shape of an 'L' on her forehead", iterations = 100, source_lang = default_source_language, target_lang = 'zh-CN', key = key, print_intermediates = TRUE)