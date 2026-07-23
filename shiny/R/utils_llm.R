# shiny/R/utils_llm.R – version avec fallback Groq → Gemini → Ollama → scripté

`%||%` <- function(x, y) if (is.null(x)) y else x

construire_prompt <- function(question, contexte_chat, historique = NULL) {
  messages <- list()
  messages <- append(messages, list(list(
    role = "user",
    parts = list(list(text = contexte_chat))
  )))
  if (!is.null(historique) && nrow(historique) > 0) {
    n_hist <- min(nrow(historique), 6)
    for (i in seq_len(n_hist)) {
      role <- if (historique$role[i] == "user") "user" else "model"
      messages <- append(messages, list(list(
        role = role,
        parts = list(list(text = historique$content[i]))
      )))
    }
  }
  messages <- append(messages, list(list(
    role = "user",
    parts = list(list(text = question))
  )))
  list(contents = messages)
}

# Nouvelle fonction : appel à Groq (API compatible OpenAI, très rapide)
appeler_groq <- function(prompt, cle_api, modele = "llama-3.3-70b-versatile") {
  if (is.null(cle_api) || cle_api == "") return(NULL)
  
  url <- "https://api.groq.com/openai/v1/chat/completions"
  
  # Conversion du prompt Gemini (role/parts) vers format OpenAI/Groq (role/content)
  messages_gemini <- prompt$contents
  messages_groq <- lapply(messages_gemini, function(msg) {
    role <- if (msg$role == "user") "user" else "assistant"
    list(role = role, content = msg$parts[[1]]$text)
  })
  
  body <- list(
    model = modele,
    messages = messages_groq,
    temperature = 0.6,
    max_tokens = 500
  )
  
  response <- tryCatch(
    httr::POST(url,
               httr::add_headers(Authorization = paste("Bearer", cle_api)),
               body = jsonlite::toJSON(body, auto_unbox = TRUE),
               encode = "json", httr::content_type("application/json"), httr::timeout(15)),
    error = function(e) NULL
  )
  if (is.null(response) || httr::status_code(response) != 200) return(NULL)
  
  contenu <- httr::content(response, "parsed", encoding = "UTF-8")
  texte <- contenu$choices[[1]]$message$content
  if (is.null(texte) || texte == "") return(NULL)
  texte
}

appeler_gemini <- function(prompt, cle_api) {
  if (is.null(cle_api) || cle_api == "") return(NULL)
  url <- paste0(
    "https://generativelanguage.googleapis.com/v1beta/models/gemini-3-flash-preview:generateContent?key=",
    cle_api
  )
  response <- tryCatch(
    httr::POST(url, body = jsonlite::toJSON(prompt, auto_unbox = TRUE),
               encode = "json", httr::content_type("application/json"), httr::timeout(15)),
    error = function(e) NULL
  )
  if (is.null(response) || httr::status_code(response) != 200) return(NULL)
  contenu <- httr::content(response, "parsed", encoding = "UTF-8")
  texte <- contenu$candidates[[1]]$content$parts[[1]]$text
  if (is.null(texte) || texte == "") return(NULL)
  texte
}

# Appel à Ollama local
appeler_ollama <- function(prompt, modele = "llama3.1:8b") {
  url <- "http://localhost:11434/api/generate"
  
  messages <- prompt$contents
  texte_prompt <- ""
  for (msg in messages) {
    role <- if (msg$role == "user") "Utilisateur" else "Assistant"
    texte_prompt <- paste0(texte_prompt, role, ": ", msg$parts[[1]]$text, "\n\n")
  }
  texte_prompt <- paste0(texte_prompt, "Assistant: ")
  
  body <- list(
    model = modele,
    prompt = texte_prompt,
    stream = FALSE,
    options = list(temperature = 0.7, num_predict = 500)
  )
  
  response <- tryCatch(
    httr::POST(url, body = jsonlite::toJSON(body, auto_unbox = TRUE),
               encode = "json", httr::timeout(30)),
    error = function(e) NULL
  )
  if (is.null(response) || httr::status_code(response) != 200) return(NULL)
  contenu <- httr::content(response, "parsed", encoding = "UTF-8")
  return(contenu$response)
}

# Fonction de fallback scripté
repondre_scriptee <- function(question, dash) {
  return("Désolé, les services d'IA sont temporairement indisponibles. Tu peux explorer les onglets du tableau de bord pour trouver les réponses.")
}

# Fonction principale avec fallback Groq → Gemini → Ollama → script
repondre_question <- function(question, dash, contexte_chat, cle_groq, cle_gemini, historique = NULL) {
  prompt <- construire_prompt(question, contexte_chat, historique)
  
  # Essayer Groq (rapide, en priorité)
  reponse <- appeler_groq(prompt, cle_groq)
  if (!is.null(reponse)) return(reponse)
  
  # Essayer Gemini
  reponse <- appeler_gemini(prompt, cle_gemini)
  if (!is.null(reponse)) return(reponse)
  
  # Essayer Ollama (local)
  reponse <- appeler_ollama(prompt)
  if (!is.null(reponse)) return(reponse)
  
  # Fallback scripté
  return(repondre_scriptee(question, dash))
}