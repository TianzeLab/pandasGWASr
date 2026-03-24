
ask_yes_no_question <- function(question) {
  yes_no_answer <- ""
  while (toupper(yes_no_answer) != "YES" && toupper(yes_no_answer) != "NO") {
    yes_no_answer <- readline(question)
  }
  return(toupper(yes_no_answer))
}
