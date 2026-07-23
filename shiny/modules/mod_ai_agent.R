# shiny/modules/mod_ai_agent.R
# Module Shiny pour l'agent conversationnel (Groq → Gemini → Ollama → scripté)
# ------------------------------------------------------------------------------
# UI du module
# ------------------------------------------------------------------------------
ai_agent_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "chat-container",
        div(class = "chat-history", id = ns("chat_history"),
            uiOutput(ns("chat_messages"))
        ),
        div(class = "chat-input-area",
            fluidRow(
              column(9, textAreaInput(ns("question"), label = NULL,
                                      placeholder = "Pose ta question sur la filière mil...",
                                      width = "100%", rows = 2)),
              column(3, actionButton(ns("btn_send"), NULL, icon = bs_icon("send-fill"),
                                     class = "btn btn-primary btn-sm mt-2"))
            )
        )
    )
  )
}

ai_agent_server <- function(id, contexte_chat, cle_groq, cle_gemini) {
  moduleServer(id, function(input, output, session) {
    historique <- reactiveVal(
      data.frame(role = character(0), content = character(0), stringsAsFactors = FALSE)
    )
    
    output$chat_messages <- renderUI({
      msgs <- historique()
      if (nrow(msgs) == 0) {
        return(
          div(class = "chat-message assistant",
              div(class = "chat-bubble",
                  "Bonjour ! Je suis Sira, ton assistante pour la filière mil au Mali. Pose-moi une question !"))
        )
      }
      lapply(seq_len(nrow(msgs)), function(i) {
        role <- msgs$role[i]
        content <- msgs$content[i]
        div(class = paste("chat-message", role),
            div(class = "chat-bubble", content))
      })
    })
    
    observeEvent(input$btn_send, {
      question <- trimws(input$question)
      if (question == "") return()
      
      hist_actuel <- historique()
      hist_actuel <- rbind(hist_actuel,
                           data.frame(role = "user", content = question, stringsAsFactors = FALSE))
      historique(hist_actuel)
      updateTextAreaInput(session, "question", value = "")
      
      reponse <- repondre_question(question, NULL, contexte_chat, cle_groq, cle_gemini, hist_actuel)
      
      hist_actuel <- historique()
      hist_actuel <- rbind(hist_actuel,
                           data.frame(role = "assistant", content = reponse, stringsAsFactors = FALSE))
      historique(hist_actuel)
    })
  })
}