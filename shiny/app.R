# =============================================================================
# app.R : Dashboard filière mil – avec titres, carte corrigée, agent IA
# =============================================================================

source(here::here("shiny", "global.R"))
source(here::here("shiny", "modules", "mod_ai_agent.R"))

tab_btn <- function(target, icon, titre, actif = FALSE) {
  tags$li(
    class = "nav-item",
    tags$button(
      class = paste("nav-link", if (actif) "active"),
      `data-bs-toggle` = "pill",
      `data-bs-target` = target,
      type = "button",
      div(bs_icon(icon, size = "1.3rem"), br(), tags$small(titre))
    )
  )
}

ui <- page_fluid(
  theme = bs_theme(version = 5, base_font = font_google("Inter")),
  
  tags$head(
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$style(HTML("
      body, .app-container {
        background-color: #f7f3ed !important;
        color: #2c2c2c;
        font-family: 'Inter', sans-serif;
      }
      .app-container { display: flex; min-height: 100vh; }
      .sidebar-tabs {
        width: 100px;
        background-color: #ffffff;
        border-right: 1px solid #e2dbd0;
        padding: 20px 5px;
        display: flex;
        flex-direction: column;
        align-items: center;
      }
      .brand-icon { font-size: 1.8rem; color: #d97706; margin-bottom: 15px; }
      .sidebar-tabs .nav-link {
        width: 85px; height: auto;
        border-radius: 14px;
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        color: #b8a98f !important;
        background: transparent;
        border: none;
        margin-bottom: 10px;
        padding: 8px 2px;
        transition: all .25s ease;
        font-size: 0.65rem;
        line-height: 1.2;
      }
      .sidebar-tabs .nav-link small {
        display: block;
        color: inherit;
        text-align: center;
      }
      .sidebar-tabs .nav-link.active {
        color: #fff !important;
        background: #d97706 !important;
        box-shadow: 0 6px 16px rgba(217,119,6,.35);
      }
      .sidebar-tabs .nav-link:hover:not(.active) {
        background: #f3ece1; color: #d97706 !important;
      }
      .main-content { flex: 1; padding: 30px; overflow-y: auto; }

      .hero-millet-card {
        position: relative; border-radius: 20px; overflow: hidden;
        height: 480px; background-size: cover; background-position: center;
        box-shadow: 0 10px 30px rgba(0,0,0,0.06);
      }
      .hero-overlay {
        position: absolute; inset: 0;
        background: linear-gradient(180deg, rgba(0,0,0,0.4) 0%, rgba(0,0,0,0.15) 50%, rgba(0,0,0,0.65) 100%);
        padding: 30px; display: flex; flex-direction: column;
        justify-content: space-between;
      }
      .glass-badge {
        background: rgba(45, 35, 25, 0.45);
        backdrop-filter: blur(14px); -webkit-backdrop-filter: blur(14px);
        border: 1px solid rgba(255, 255, 255, 0.25);
        border-radius: 30px; padding: 8px 18px; color: #ffffff;
        font-size: 0.9rem; font-weight: 500; display: inline-flex;
        align-items: center; gap: 9px; box-shadow: 0 4px 15px rgba(0,0,0,0.15);
      }
      .photo-card {
        background: #ffffff; border-radius: 18px; padding: 22px;
        box-shadow: 0 4px 20px rgba(0, 0, 0, 0.03);
        border: 1px solid #efe8dd; margin-bottom: 20px;
      }
      .kpi-label { font-weight: 600; color: #666; font-size: 0.9rem; }
      .kpi-value { font-size: 1.8rem; font-weight: 700; color: #d97706; }
      .page-title { font-weight: 700; color: #2a2a2a; }
      .page-subtitle { color: #666; margin-bottom: 25px; }
      .synth-block {
        background: #fdf8f3; border-left: 4px solid #d97706;
        padding: 10px 14px; margin-bottom: 8px; border-radius: 6px;
      }
      /* Chat */
      .chat-container {
        display: flex; flex-direction: column; height: 60vh;
        border: 1px solid #efe8dd; border-radius: 12px; overflow: hidden;
      }
      .chat-history {
        flex: 1; overflow-y: auto; padding: 15px; background-color: #fdf8f3;
      }
      .chat-message { margin-bottom: 12px; display: flex; }
      .chat-message.user { justify-content: flex-end; }
      .chat-message.assistant { justify-content: flex-start; }
      .chat-bubble {
        max-width: 75%; padding: 10px 15px; border-radius: 18px;
        font-size: 0.9rem; line-height: 1.4;
      }
      .chat-message.user .chat-bubble {
        background-color: #d97706; color: white; border-bottom-right-radius: 4px;
      }
      .chat-message.assistant .chat-bubble {
        background-color: #f3ece1; color: #2c2c2c; border-bottom-left-radius: 4px;
      }
      .chat-input-area {
        padding: 15px; border-top: 1px solid #efe8dd; background-color: #ffffff;
      }
    "))
  ),
  
  div(class = "app-container",
      
      div(class = "sidebar-tabs",
          div(class = "brand-icon", bs_icon("grid-1x2-fill")),
          tags$ul(class = "nav nav-pills flex-column", role = "tablist",
                  tab_btn("#tab1", "star-fill",             "Accueil", actif = TRUE),
                  tab_btn("#tab2", "people-fill",           "Profil"),
                  tab_btn("#tab3", "bar-chart-line-fill",   "Rendement"),
                  tab_btn("#tab4", "cash-stack",            "Prix"),
                  tab_btn("#tab5", "shield-fill-check",     "Sécurité"),
                  tab_btn("#tab6", "journal-text",          "Méthode"),
                  tab_btn("#tab7", "robot",                 "Assistant")
          )
      ),
      
      div(class = "main-content",
          div(class = "tab-content",
              
              # ===== ONGLET 1 : ACCUEIL & IMPORTANCE =====
              div(class="tab-pane fade show active", id="tab1",
                  layout_columns(
                    col_widths = c(8, 4),
                    div(class = "hero-millet-card",
                        style = "background-image: url('mil_grains.jpg');",
                        div(class = "hero-overlay",
                            div(
                              h1("Le mil au Mali", style="color:white; font-weight:700; font-size:2.3rem;"),
                              p("1re source céréalière et base de la sécurité alimentaire nationale",
                                style="color:rgba(255,255,255,0.9); font-size:1.05rem;")
                            ),
                            div(style = "display:flex; flex-wrap:wrap; gap:10px;",
                                div(class="glass-badge", bs_icon("cloud-drizzle"), "Pluviométrie: 450-600 mm"),
                                div(class="glass-badge", bs_icon("lightning-charge"), "Calories: 507 kcal/hab/j"),
                                div(class="glass-badge", bs_icon("people"), "Producteurs: 29,4% (1er)"),
                                div(class="glass-badge", bs_icon("aspect-ratio"), "Superficie: 3,1 Mha"),
                                div(class="glass-badge", bs_icon("house-heart"), "Autoconsommation: 95%")
                            )
                        )
                    ),
                    div(
                      div(class = "photo-card",
                          h5("Comparer les céréales", style="font-weight:600;"),
                          selectInput("cereale_var", "Indicateur :",
                                      choices = c("Prévalence conso (%)"="prevalence",
                                                  "Part des calories (%)"="part_cal",
                                                  "Producteurs (%)"="producteurs",
                                                  "Taux de commercialisation (%)"="taux_commerc",
                                                  "Part autoconsommée (%)"="autoconso"),
                                      selected = "producteurs"),
                          p("Le mil domine la production et l'autoconsommation, le riz domine la consommation urbaine.",
                            style="color:#777; font-size:0.85rem;")
                      ),
                      div(class = "photo-card",
                          h5(textOutput("titre_cereales"), style="font-size:0.95rem; font-weight:600;"),
                          plotlyOutput("plot_cereales", height="230px")
                      )
                    )
                  ),
                  br(),
                  layout_columns(
                    col_widths = c(6, 6),
                    div(class = "photo-card",
                        h4("Souveraineté alimentaire : production vs importations", style="font-size:1.05rem; font-weight:600;"),
                        plotlyOutput("plot_fao_commerce", height="280px"),
                        p("Le mil est produit localement sans importation, quand le riz dépend des importations à 17%.",
                          style="color:#666; font-size:0.85rem;")
                    ),
                    div(class = "photo-card",
                        h4("Disponibilité par habitant (kg/an)", style="font-size:1.05rem; font-weight:600;"),
                        plotlyOutput("plot_fao_dispo", height="280px"),
                        p("Le Mali est l'un des plus gros consommateurs de mil par habitant au monde.",
                          style="color:#666; font-size:0.85rem;")
                    )
                  ),
                  layout_columns(
                    col_widths = c(7, 5),
                    div(class = "photo-card",
                        h4("Vulnérabilité climatique : gradient pluviométrique nord-sud", style="font-size:1.05rem; font-weight:600;"),
                        plotlyOutput("plot_pluie", height="280px"),
                        p("Le mil est cultivé dans les régions semi-arides du centre (Mopti, Ségou), où la pluviométrie est faible et variable. Culture strictement pluviale, il est exposé directement aux aléas climatiques.",
                          style="color:#666; font-size:0.85rem;")
                    ),
                    div(class = "photo-card",
                        h4("Synthèse : quatre raisons de choisir le mil", style="font-size:1.05rem; font-weight:600; margin-bottom:15px;"),
                        div(class="synth-block", bs_icon("patch-check"), tags$b(" Production :"), " 1re céréale (29% des ménages, 3,1 Mha)"),
                        div(class="synth-block", bs_icon("lightning-charge"), tags$b(" Calories :"), " 1re source alimentaire (FAO)"),
                        div(class="synth-block", bs_icon("box-seam"), tags$b(" Vivrier :"), " 95% autoconsommé, peu commercialisé"),
                        div(class="synth-block", bs_icon("exclamation-triangle"), tags$b(" Vulnérable :"), " pluvial, exposé au climat, non coté à l'international")
                    )
                  )
              ),
              
              # ===== ONGLET 2 : PROFIL DES MENAGES =====
              div(class="tab-pane fade", id="tab2",
                  h2("Profil des ménages", class="page-title"),
                  p("Typologie croisant production et consommation de mil", class="page-subtitle"),
                  layout_columns(col_widths=c(3,9),
                                 div(class="photo-card",
                                     h5("Filtres"),
                                     selectInput("f_milieu","Milieu :",
                                                 choices=c("Tous"="tous","Urbain"="1","Rural"="2")),
                                     selectInput("f_region","Région :",
                                                 choices=c("Toutes"="tous", setNames(as.character(1:9),
                                                                                     c("Kayes","Koulikoro","Sikasso","Ségou","Mopti",
                                                                                       "Tombouctou","Gao","Kidal","Bamako")))),
                                     selectInput("f_quintile","Quintile de vie :",
                                                 choices=c("Tous"="tous", setNames(as.character(1:5), paste("Quintile",1:5)))),
                                     hr(),
                                     textOutput("n_menages_filtre")
                                 ),
                                 div(class="photo-card",
                                     h4("Répartition des 4 groupes de la filière"),
                                     plotlyOutput("plot_groupes", height="330px"))
                  ),
                  layout_columns(col_widths=c(7,5),
                                 div(class="photo-card",
                                     h4("Niveau de vie par groupe"),
                                     plotlyOutput("plot_niveauvie", height="300px"),
                                     p("Les producteurs de mil sont nettement les plus pauvres, avec un niveau de vie près de deux fois inférieur aux consommateurs non producteurs.",
                                       style="color:#666; font-size:0.85rem;")),
                                 div(class="photo-card",
                                     h4("Caractéristiques moyennes"),
                                     tableOutput("tab_caract"))
                  ),
                  div(class="photo-card",
                      h4("Ruralité et alphabétisation par groupe"),
                      plotlyOutput("plot_rural_alpha", height="320px"),
                      p("Les producteurs de mil sont quasi exclusivement ruraux (97%) et les moins alphabétisés (33%). Le mil est cultivé par les ménages ruraux les plus modestes, tandis que les consommateurs non producteurs, plus urbains et instruits, achètent leur mil.",
                        style="color:#666; font-size:0.85rem;")
                  ),
                  div(class="photo-card",
                      h4("Une filière socialement stratifiée"),
                      p("Le mil oppose deux mondes. D'un côté, les producteurs, ruraux, pauvres, peu alphabétisés, à grandes familles, qui cultivent le mil pour se nourrir. De l'autre, les consommateurs non producteurs, plus urbains, aisés et instruits, qui achètent leur mil. Cette stratification traverse toute la filière et éclaire les analyses de sécurité alimentaire.")
                  )
              ),
              
              # ===== ONGLET 3 : PRODUCTION ET RENDEMENTS =====
              div(class="tab-pane fade", id="tab3",
                  h2("Production et rendements", class="page-title"),
                  p("Rendements du mil, distribution spatiale et déterminants", class="page-subtitle"),
                  layout_columns(col_widths=c(4,4,4),
                                 div(class="photo-card", div(class="kpi-label","Rendement moyen"),
                                     div(class="kpi-value","586"), p("kg/ha (pondéré)",style="color:#999;")),
                                 div(class="photo-card", div(class="kpi-label","Rendement médian"),
                                     div(class="kpi-value","381"), p("kg/ha",style="color:#999;")),
                                 div(class="photo-card", div(class="kpi-label","Superficie moyenne"),
                                     div(class="kpi-value","3,5"), p("ha/ménage",style="color:#999;"))
                  ),
                  layout_columns(col_widths=c(5,7),
                                 div(class="photo-card",
                                     h4(textOutput("titre_rendement"), style="font-size:1.05rem; font-weight:600;"),
                                     sliderInput("wins_seuil","Plafond de rendement (kg/ha) :",
                                                 min=1000, max=5000, value=5000, step=500),
                                     plotlyOutput("plot_rendement", height="280px"),
                                     p("Le curseur ajuste le plafond physiologique appliqué à la production.",
                                       style="color:#777; font-size:0.85rem;")),
                                 div(class="photo-card", h4("Rendement moyen par région"),
                                     leafletOutput("carte_rendement", height="400px"))
                  ),
                  div(class="photo-card",
                      h4("Déterminants du rendement (régression)"),
                      plotlyOutput("plot_det_rendement", height="320px"),
                      p("Deux facteurs augmentent significativement le rendement : la dépense en intrants et la fertilité du sol. Les semences améliorées, l'irrigation et les caractéristiques du chef de ménage n'ont pas d'effet significatif, ces pratiques étant rares. L'essentiel des écarts de rendement provient de la géographie.",
                        style="color:#666; font-size:0.85rem;")
                  ),
                  div(class="photo-card",
                      h4("Une production pluviale peu intensifiée"),
                      p("Le rendement moyen du mil, environ 586 kg/ha, reste modeste, cohérent avec une agriculture de subsistance. La production repose sur des semences locales (95%), sans irrigation (culture pluviale à 98%), avec un faible recours aux intrants. Les leviers d'amélioration sont l'intensification en intrants et la valorisation de la fertilité des sols.")
                  )
              ),
              
              # ===== ONGLET 4 : CHAINE DES PRIX =====
              div(class="tab-pane fade", id="tab4",
                  h2("Chaîne des prix", class="page-title"),
                  p("Du producteur au consommateur : formation des prix et marges", class="page-subtitle"),
                  layout_columns(col_widths=c(3,3,3,3),
                                 div(class="photo-card", div(class="kpi-label","Prix producteur"),
                                     div(class="kpi-value","200"), p("FCFA/kg",style="color:#999;")),
                                 div(class="photo-card", div(class="kpi-label","Prix marché (INSTAT)"),
                                     div(class="kpi-value","375"), p("FCFA/kg",style="color:#999;")),
                                 div(class="photo-card", div(class="kpi-label","Marge"),
                                     div(class="kpi-value","175"), p("FCFA/kg",style="color:#999;")),
                                 div(class="photo-card", div(class="kpi-label","Part producteur"),
                                     div(class="kpi-value","53%"), p("du prix final",style="color:#999;"))
                  ),
                  div(class="photo-card",
                      h4("Validation : deux sources de prix concordantes"),
                      plotlyOutput("plot_valid_prix", height="300px"),
                      p("Le prix consommateur reconstitué à partir des achats des ménages (350 FCFA/kg) concorde étroitement avec les relevés de prix de marché officiels de l'INSTAT (375 FCFA/kg). Cette convergence entre deux sources indépendantes confirme la fiabilité de l'analyse.",
                        style="color:#666; font-size:0.85rem;")
                  ),
                  div(class="photo-card",
                      h4("Marge de commercialisation par région"),
                      leafletOutput("carte_marge", height="380px"),
                      p("La marge est la plus forte dans les grands bassins de production comme Mopti, où le producteur ne capte que 55% du prix final. Les zones de forte production sont paradoxalement celles où le producteur est le moins bien rémunéré.",
                        style="color:#666; font-size:0.85rem;")
                  ),
                  layout_columns(col_widths=c(6,6),
                                 div(class="photo-card", h4("Canaux de vente"),
                                     plotlyOutput("plot_canaux", height="280px"),
                                     p("Vente informelle, aucune coopérative.", style="color:#777; font-size:0.85rem;")),
                                 div(class="photo-card", h4("Méthodes de stockage"),
                                     plotlyOutput("plot_stockage", height="280px"),
                                     p("Stockage traditionnel majoritaire, exposé aux pertes.", style="color:#777; font-size:0.85rem;"))
                  ),
                  div(class="photo-card",
                      h4("Une filière commerciale informelle"),
                      p("Le mil est peu commercialisé (moins de 5% de la production vendue). La vente se fait de façon informelle, à des particuliers ou sur le marché local, sans coopérative. Le stockage traditionnel en grenier expose la production aux pertes post-récolte. Le producteur capte un peu plus de la moitié du prix final, laissant une marge substantielle à une chaîne peu structurée.")
                  )
              ),
              
              # ===== ONGLET 5 : SECURITE ALIMENTAIRE =====
              div(class="tab-pane fade", id="tab5",
                  h2("Sécurité alimentaire", class="page-title"),
                  p("Insécurité (FIES), diversité (HDDS) et impact de la filière", class="page-subtitle"),
                  layout_columns(col_widths=c(4,4,4),
                                 div(class="photo-card", div(class="kpi-label","Insécurité (nat.)"),
                                     div(class="kpi-value","17,5%"), p("modérée ou sévère",style="color:#999;")),
                                 div(class="photo-card", div(class="kpi-label","Insécurité sévère"),
                                     div(class="kpi-value","5,5%"), p("des ménages",style="color:#999;")),
                                 div(class="photo-card", div(class="kpi-label","Diversité (HDDS)"),
                                     div(class="kpi-value","9,3"), p("groupes / 12",style="color:#999;"))
                  ),
                  layout_columns(col_widths=c(3,9),
                                 div(class="photo-card",
                                     h5("Indicateur"),
                                     radioButtons("secu_outcome","Afficher :",
                                                  choices=c("Insécurité (FIES)"="fies","Diversité (HDDS)"="hdds"),
                                                  selected="fies"),
                                     p("Compare les 4 groupes de la filière.", style="color:#777; font-size:0.85rem;")),
                                 div(class="photo-card",
                                     h4(textOutput("titre_secu"), style="font-size:1.05rem; font-weight:600;"),
                                     plotlyOutput("plot_secu", height="330px"))
                  ),
                  div(class="photo-card",
                      h4("Un arbitrage entre sécurité et diversité"),
                      p("Les producteurs de mil connaissent plus d'insécurité modérée (pauvreté) mais moins d'insécurité sévère : leur autoproduction agit comme un filet de sécurité contre la faim extrême. En revanche, leur régime est moins diversifié. À l'inverse, les ménages dépendants du marché ont un régime plus varié mais sont plus exposés aux chocs sévères. L'autoproduction assure la subsistance, pas la diversité.")
                  ),
                  div(class="photo-card",
                      h4(textOutput("titre_impact"), style="font-size:1.05rem; font-weight:600;"),
                      plotlyOutput("plot_coefs", height="300px"),
                      p("Une fois le niveau de vie contrôlé, produire du mil n'a aucun effet significatif sur la sécurité alimentaire : les difficultés des producteurs relèvent de leur pauvreté, non de la culture. Le niveau de vie est le déterminant écrasant. Le mil est une activité de subsistance neutre, ni piège ni solution miracle.",
                        style="color:#666; font-size:0.85rem;")
                  )
              ),
              
              # ===== ONGLET 6 : METHODOLOGIE =====
              div(class="tab-pane fade", id="tab6",
                  h2("Méthodologie et contexte", class="page-title"),
                  p("Choix méthodologiques et éléments de contexte", class="page-subtitle"),
                  layout_columns(col_widths=c(6,6),
                                 div(class="photo-card",
                                     h4("Source des données"),
                                     p("EHCVM-2 2021/22 (Enquête Harmonisée sur les Conditions de Vie des Ménages), 6 143 ménages agricoles. Données pondérées pour représentativité nationale."),
                                     h5("Reconstitution de la production"),
                                     p("La récolte directe (S16C) étant peu renseignée pour le mil, la production a été reconstituée via les usages (S16D) : consommation, dons, ventes et stock.")
                                 ),
                                 div(class="photo-card",
                                     h4("Traitement des rendements"),
                                     p("Plafonnement physiologique (plafond du mil : 5 000 kg/ha, Crop Trust), écartement des rendements hors stock aberrants, winsorisation par strate région-milieu."),
                                     h5("Indicateurs de sécurité alimentaire"),
                                     p("FIES : échelle FAO à 8 questions (modérée si score supérieur ou égal à 4, sévère si supérieur ou égal à 7). HDDS : diversité sur 12 groupes alimentaires.")
                                 )
                  ),
                  div(class="photo-card",
                      h4("Prix et validation croisée"),
                      p("Les prix producteurs et consommateurs ont été reconstitués à partir des transactions déclarées, puis validés par les relevés de prix de marché officiels de l'INSTAT (fichier ehcvm_prix). Les deux sources concordent, confortant la robustesse des marges.")
                  ),
                  div(class="photo-card",
                      h4("Limites"),
                      p("La transformation du mil n'est pas captée par l'enquête (angle mort). Les résultats d'impact sont associatifs, non causaux (données transversales).")
                  ),
                  div(class="photo-card",
                      h4("Message clé"),
                      p("Le mil est une céréale vivrière des ménages ruraux modestes du centre du pays. Sa production dépend surtout des intrants et de la fertilité des sols. Peu commercialisé, il joue un rôle de subsistance : une fois la pauvreté prise en compte, il n'aggrave ni n'améliore la sécurité alimentaire, mais l'autoproduction protège de la faim sévère.")
                  )
              ),
              
              # ===== ONGLET 7 : ASSISTANT IA =====
              div(class="tab-pane fade", id="tab7",
                  h2("Sira – Assistant virtuel", class="page-title"),
                  p("Posez une question sur la filière mil. Sira s'appuie sur les données du dashboard et un modèle d'intelligence artificielle pour vous répondre.", class="page-subtitle"),
                  ai_agent_ui("chat_agent")
              )
          )
      )
  )
)

server <- function(input, output, session) {
  
  # Agent conversationnel
  ai_agent_server("chat_agent", contexte_chat, groq_api_key, gemini_api_key)
  
  style_plotly <- function(p) {
    p %>% layout(paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
                 font=list(color="#333333"),
                 xaxis=list(gridcolor="rgba(0,0,0,0.06)"),
                 yaxis=list(gridcolor="rgba(0,0,0,0.06)"),
                 legend=list(font=list(color="#333333")))
  }
  
  # ----- Onglet 1 -----
  labels_cereales <- c(prevalence="Prévalence de consommation (%)",
                       part_cal="Part dans les calories (%)",
                       producteurs="Ménages producteurs (%)",
                       taux_commerc="Taux de commercialisation (%)",
                       autoconso="Part autoconsommée (%)")
  output$titre_cereales <- renderText({ labels_cereales[[input$cereale_var]] })
  output$plot_cereales <- renderPlotly({
    d <- dash$comparaison_cereales
    d$val <- d[[input$cereale_var]]
    coul <- ifelse(d$cereale=="Mil", couleur_mil, "#c9bda8")
    plot_ly(d, x=~reorder(cereale,-val), y=~val, type="bar",
            marker=list(color=coul)) %>%
      layout(xaxis=list(title=""), yaxis=list(title="")) %>% style_plotly()
  })
  output$plot_fao_commerce <- renderPlotly({
    d <- dash$fao_commerce
    plot_ly(d, x=~produit) %>%
      add_bars(y=~Production, name="Production", marker=list(color=couleur_mil)) %>%
      add_bars(y=~`Import Quantity`, name="Importations", marker=list(color=couleur_accent)) %>%
      layout(barmode="group", xaxis=list(title=""), yaxis=list(title="Milliers de tonnes")) %>% style_plotly()
  })
  output$plot_fao_dispo <- renderPlotly({
    d <- dash$fao_dispo
    coul <- ifelse(d$produit=="Mil", couleur_mil, "#c9bda8")
    plot_ly(d, x=~reorder(produit,-kg_capita_an), y=~kg_capita_an, type="bar",
            marker=list(color=coul)) %>%
      layout(xaxis=list(title=""), yaxis=list(title="kg/hab/an")) %>% style_plotly()
  })
  output$plot_pluie <- renderPlotly({
    d <- dash$pluie_region
    d <- d[!is.na(d$region_nom),]
    plot_ly(d, x=~reorder(region_nom, pluie_moyenne), y=~pluie_moyenne,
            type="bar", color=~cereale_dominante,
            colors=c("Mil"=couleur_mil,"Riz"="#5b8fb0","Sorgho"="#c17d3a","Maïs"="#7fae5b")) %>%
      layout(xaxis=list(title="Région (du plus sec au plus humide)"),
             yaxis=list(title="Pluie moyenne (mm)"),
             legend=list(title=list(text="Céréale dominante"))) %>% style_plotly()
  })
  
  # ----- Onglet 2 -----
  menages_filtres <- reactive({
    d <- dash$menages
    if (input$f_milieu != "tous") d <- d[as.character(d$milieu)==input$f_milieu,]
    if (input$f_region != "tous") d <- d[as.character(d$region)==input$f_region,]
    if (input$f_quintile != "tous") d <- d[as.character(d$quintile)==input$f_quintile,]
    d
  })
  output$plot_groupes <- renderPlotly({
    d <- menages_filtres()
    if (nrow(d)==0) return(plotly_empty())
    tab <- d %>% group_by(groupe) %>%
      summarise(part=100*sum(hhweight)/sum(d$hhweight), .groups="drop")
    plot_ly(tab, x=~part, y=~reorder(groupe,part), type="bar", orientation="h",
            marker=list(color=couleur_mil)) %>%
      layout(xaxis=list(title="% des ménages"), yaxis=list(title="")) %>% style_plotly()
  })
  output$n_menages_filtre <- renderText({ paste0(nrow(menages_filtres()), " ménages sélectionnés") })
  output$plot_niveauvie <- renderPlotly({
    d <- dash$caract_groupes
    plot_ly(d, x=~niveau_vie, y=~reorder(groupe,niveau_vie), type="bar",
            orientation="h", marker=list(color=couleur_mil)) %>%
      layout(xaxis=list(title="Niveau de vie (FCFA/tête)"), yaxis=list(title="")) %>% style_plotly()
  })
  output$tab_caract <- renderTable({
    dash$caract_groupes %>%
      transmute(Groupe=groupe, `Âge chef`=round(age_chef), Taille=round(taille),
                `Alpha. %`=round(alphabet), `Rural %`=round(rural))
  })
  output$plot_rural_alpha <- renderPlotly({
    d <- dash$caract_groupes
    plot_ly(d, x=~groupe) %>%
      add_bars(y=~rural, name="Rural %", marker=list(color=couleur_mil)) %>%
      add_bars(y=~alphabet, name="Alphabétisé %", marker=list(color="#5b8fb0")) %>%
      layout(barmode="group", xaxis=list(title=""), yaxis=list(title="%")) %>% style_plotly()
  })
  
  # ----- Onglet 3 -----
  output$titre_rendement <- renderText({
    paste0("Distribution des rendements (plafond ", input$wins_seuil, " kg/ha)")
  })
  output$plot_rendement <- renderPlotly({
    r <- pmin(dash$rendement_menage$rendement_wins, input$wins_seuil)
    plot_ly(x=~r, type="histogram", marker=list(color=couleur_mil)) %>%
      layout(xaxis=list(title="Rendement (kg/ha)"), yaxis=list(title="Ménages")) %>% style_plotly()
  })
  output$carte_rendement <- renderLeaflet({
    d <- dash$rendement_par_region %>%
      mutate(region_nom = as.character(region))
    carte <- mali_regions %>%
      left_join(d, by = c("NAME_1" = "region_nom"))
    pal <- colorNumeric("YlOrBr", domain = carte$rendement, na.color = "#e0e0e0")
    leaflet(carte) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor   = ~pal(rendement),
        fillOpacity = 0.8,
        color       = "white",
        weight      = 1,
        label       = ~paste0(NAME_1, " : ", round(rendement), " kg/ha")
      ) %>%
      addLegend(pal = pal, values = ~rendement, title = "kg/ha")
  })
  output$plot_det_rendement <- renderPlotly({
    d <- dash$coefs_rendement
    d <- d[d$variable != "(Intercept)",]
    plot_ly(d, x=~coef, y=~reorder(variable,coef), type="bar", orientation="h",
            marker=list(color=ifelse(d$coef>0, couleur_mil, "#c0392b"))) %>%
      layout(xaxis=list(title="Coefficient"), yaxis=list(title="")) %>% style_plotly()
  })
  
  
  
  # ----- Onglet 4 -----
  output$plot_valid_prix <- renderPlotly({
    d <- dash$marges_off %>%
      mutate(region = as.numeric(haven::zap_labels(region))) %>%
      left_join(corresp_region, by = "region") %>%
      filter(!is.na(prix_conso))
    plot_ly(d, x=~NAME_1) %>%
      add_bars(y=~prix_conso, name="Reconstitué (ménages)", marker=list(color=couleur_mil)) %>%
      add_bars(y=~prix_marche, name="Marché officiel (INSTAT)", marker=list(color="#5b8fb0")) %>%
      layout(barmode="group", xaxis=list(title=""), yaxis=list(title="FCFA/kg")) %>% style_plotly()
  })
  
  output$carte_marge <- renderLeaflet({
    d <- dash$marges_off %>%
      mutate(region = as.numeric(haven::zap_labels(region))) %>%
      left_join(corresp_region, by = "region")
    carte <- mali_regions %>% left_join(d, by = "NAME_1")
    pal <- colorNumeric("Oranges", carte$marge_marche, na.color="#e0e0e0")
    leaflet(carte) %>% addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillColor=~pal(marge_marche), fillOpacity=0.8, color="white",
                  weight=1, label=~paste0(NAME_1,": marge ", round(marge_marche)," FCFA/kg")) %>%
      addLegend(pal=pal, values=~marge_marche, title="Marge FCFA/kg")
  })
  
  output$plot_canaux <- renderPlotly({
    d <- dash$canaux
    plot_ly(d, labels=~canal, values=~part, type="pie",
            marker=list(colors=c(couleur_mil,"#5b8fb0","#c17d3a","#c9bda8"))) %>%
      layout(showlegend=TRUE) %>% style_plotly()
  })
  output$plot_stockage <- renderPlotly({
    d <- dash$stockage
    plot_ly(d, x=~part, y=~reorder(methode,part), type="bar", orientation="h",
            marker=list(color=couleur_mil)) %>%
      layout(xaxis=list(title="% des ménages"), yaxis=list(title="")) %>% style_plotly()
  })
  
  # ----- Onglet 5 -----
  output$titre_secu <- renderText({
    if (input$secu_outcome=="fies") "Insécurité alimentaire par groupe (%)"
    else "Diversité alimentaire par groupe (HDDS)"
  })
  output$titre_impact <- renderText({
    paste0("Déterminants de ",
           if (input$secu_outcome=="fies") "l'insécurité (FIES)" else "la diversité (HDDS)")
  })
  output$plot_secu <- renderPlotly({
    d <- dash$secu_par_groupe
    if (input$secu_outcome=="fies") {
      plot_ly(d, x=~reorder(groupe,prev_moderee)) %>%
        add_bars(y=~prev_moderee, name="Modérée", marker=list(color=couleur_mil)) %>%
        add_bars(y=~prev_severe, name="Sévère", marker=list(color=couleur_accent)) %>%
        layout(barmode="group", xaxis=list(title=""), yaxis=list(title="%")) %>% style_plotly()
    } else {
      plot_ly(d, x=~hdds_moyen, y=~reorder(groupe,hdds_moyen), type="bar",
              orientation="h", marker=list(color=couleur_mil)) %>%
        layout(xaxis=list(title="HDDS (0-12)"), yaxis=list(title="")) %>% style_plotly()
    }
  })
  output$plot_coefs <- renderPlotly({
    d <- dash$coefs
    d <- d[d$modele == toupper(input$secu_outcome),]
    d <- d[d$variable != "(Intercept)",]
    plot_ly(d, x=~coef, y=~reorder(variable,coef), type="bar", orientation="h",
            marker=list(color=ifelse(d$coef>0, couleur_accent, couleur_mil))) %>%
      layout(xaxis=list(title="Coefficient"), yaxis=list(title="")) %>% style_plotly()
  })
}

shinyApp(ui, server)