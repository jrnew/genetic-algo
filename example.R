#' Download speech, then process it and return data about it.
#' 
#' Download speech from URL, format it in various forms, then get data about it, 
#' including number of words/sentences, average word/sentence length, 
#' number of word/phrase occurences of given words/phrases.
#' 
#' @param words_to_count A character vector containing non-case-sensitive words to count.
#' @param words_to_count_cs A character vector containing case-sensitive words to count.
#' @param phrases_to_count A character vector containing non-case-sensitive phrases to count.
#' @param phrases_to_count_cs A character vector containing case-sensitive phrases to count.
#' @return A list object containing: 
#' \describe{
#'   \item{\code{text_formatted}}{The full formatted speech text.}
#'   \item{\code{text_words}}{A character vector with words of the speech as individual elements.}
#'   \item{\code{text_sentences}}{A character vector with sentences of the speech as individual elements.}
#'   \item{\code{speech_data_scalar}}{Scalar data about the speech, which also include
#'   \describe{
#'      \item{\code{president}}{President who made the speech.}
#'      \item{\code{year}}{Year the speech was made.}
#'      \item{\code{count_audience_response}}{Numeric vector with number of times
#'      the audience laughed or applauded.}
#'   }}
#'   \item{\code{speech_data_vector}}{Vector data about the speech.}
#' }
GetAndProcessSpeech <- function(
  url_speech,
  words_to_count = c("we", "you", "they", "free", "war", "god",
                     "republic[[:punct:] ]", "[0-9]{1,}"),
  words_to_count_cs = c("I", "America", "Republican", "Democrat", 
                        "democra", "Jesus|Christ|Christian"),
  phrases_to_count = c("god bless"),
  phrases_to_count_cs = NULL
) {
  speech_list <- GetSpeech(url_speech)
  
  # Get plain speech text in the form of a character vector of words/sentences
  text_words <- ExtractWordsFromSpeech(speech_list$text_formatted)
  text_sentences <- ExtractSentencesFromSpeech(speech_list$text_formatted)
  
  # Get speech-related data
  speech_data_all <- GetSpeechData(speech_list$text_formatted, text_words, text_sentences, 
                                   words_to_count, words_to_count_cs,
                                   phrases_to_count, phrases_to_count_cs)
  speech_data_scalar <- c(list(president = speech_list$president,
                               year = speech_list$year,
                               num_laughter = speech_list$count_audience_response[["num_laughter"]],
                               num_applause = speech_list$count_audience_response[["num_applause"]]),
                          speech_data_all$speech_data_scalar)
  speech_list_final <- list(text_formatted = speech_list$text_formatted,
                            text_words = text_words,
                            text_sentences = text_sentences,                        
                            speech_data_scalar = speech_data_scalar,
                            speech_data_vector = speech_data_all$speech_data_vector)
  message(paste0("Processed speech by ", speech_list_final$speech_data_scalar$president, 
                 " in ", speech_list_final$speech_data_scalar$year, "."))
  return(speech_list_final)
}
