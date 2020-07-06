bereken_resultaat <- function(p = 0.5, 
                              conf_level = 0.95, 
                              gewenste_nauwkeurigheid = 0.05, 
                              populatie = 1000000){
  
  z_laag <- qnorm((1 - conf_level) / 2, mean = 0, sd = 1)
  z_hoog <- qnorm(conf_level + (1 - conf_level)/2, mean = 0, sd = 1)
  
  # Berekening van betrouwbaarheidsinterval, formule gebruikt uit boek: "Buijs, A., statistiek om mee te werken (8e druk, blz. 246)"
  # dit is de waarde die wordt verkregen met een benaderingsformule (geldt eigenlijk pas bij n >= 200)
  n_steekproef <- ceiling(z_laag^2 * p * (1 - p) / (gewenste_nauwkeurigheid^2))

  # nagaan of een correctie moet plaatsvinden vanwege de eindige populatiegrootte
  bool_eindigepopulatie <- ifelse(n_steekproef/populatie >= 0.1, TRUE, FALSE)
  
  # wanneer de populatie eindig is dan wordt het betrouwbaarheidsinterval vermenigvuldigd met een factor
  factor <- ifelse(bool_eindigepopulatie, sqrt((populatie - n_steekproef)/(populatie - 1)), 1)
  
    # Er wordt een grafiek getoond, de x-as wil ik niet laten eindigen bij de waarde van n_steekproef
  # vandaar dat ik het domein iets uitbreid
  max_steekproef <- ceiling(n_steekproef / 500) * 500
  
  n <- c(20:max_steekproef)
  df_resultaat <- data.frame(n)
  
  if (max_steekproef < 200){
    # Berekening van betrouwbaarheidsinterval, formule gebruikt uit boek: "Buijs, A., statistiek om mee te werken (8e druk, blz. 240)"
    formule <- "Uitgebreide formule"
    df_resultaat$ondergrens <- factor * (p + z_laag^2/(2*df_resultaat$n) + z_laag * sqrt(p*(1-p)/df_resultaat$n + z_laag^2/(4*df_resultaat$n^2)))/(1 + z_laag^2/df_resultaat$n)
    df_resultaat$bovengrens <- factor * (p + z_hoog^2/(2*df_resultaat$n) + z_hoog * sqrt(p*(1-p)/df_resultaat$n + z_hoog^2/(4*df_resultaat$n^2)))/(1 + z_hoog^2/df_resultaat$n)
  } else {
    formule <- "Benaderingsformule"
    df_resultaat$ondergrens <- factor * (p + z_laag * sqrt(p*(1-p)/df_resultaat$n))
    df_resultaat$bovengrens <- factor * (p + z_hoog * sqrt(p*(1-p)/df_resultaat$n))
  } 
  
  df_resultaat$breedte <- df_resultaat$bovengrens - df_resultaat$ondergrens
  df_resultaat$verschil_met_gewenst <- (df_resultaat$breedte - gewenste_nauwkeurigheid*2)^2
  
  n_steekproef <- ifelse(formule == "Uitgebreide formule",
                         which.min(df_resultaat$verschil_met_gewenst), #opzoeken waar minimum wordt bereikt
                         n_steekproef)
  
  obj <- list(p = p, conf_level = conf_level, gewenste_nauwkeurigheid = gewenste_nauwkeurigheid, 
              formule = formule, data = df_resultaat, n_steekproef_optimaal = n_steekproef)
  class(obj) <- c("list", "resultaat")
  return(obj)
}

plot_resultaat <- function(r, ...){
  if (!"resultaat" %in% class(r)) stop("Het argument van de functie plot_resultaat() moet van de klasse resultaat zijn.")
  
  ggplot(data = r$data, mapping = aes(x = n), ...) +
    geom_line(aes(y = ondergrens, color = "ondergrens"), size = 2) +
    geom_line(aes(y = bovengrens, color = "bovengrens"), size = 2) + 
    geom_vline(aes(xintercept = r$n_steekproef_optimaal, color = "optimale steekproefgrootte"), linetype = "dashed", size = 1) +
    geom_label(aes(x = r$n_steekproef_optimaal, y = r$p, label = r$n_steekproef_optimaal))  +
    labs(x = "steekproefgrootte", y = "kans", title = glue("Steekproefgrootte berekend met {tolower(r$formule)}")) +
    scale_color_manual(name = "legenda",
                       values = c("ondergrens" = "darkred",
                                  "bovengrens" = "darkgreen",
                                  "optimale steekproefgrootte" = "purple"),
                       labels = c("bovengrens",
                                  "ondergrens",
                                  "optimale steekproefgrootte"))
}

beschrijf_resultaat <- function(r){
  if (!"resultaat" %in% class(r)) 
    stop("Het argument van de functie beschrijf_resultaat() moet van de klasse resultaat zijn.")
  
  glue("{r$formule}: wanneer de kans op succes in de totale populatie {r$p} is,",
       "dan kun je met een betrouwbaarheid van {r$conf_level}",
       "zeggen dat in een steekproef van {r$n_steekproef_optimaal} stuks",
       "de kans op succes tussen {r$p - r$gewenste_nauwkeurigheid}",
       "en {r$p + r$gewenste_nauwkeurigheid} ligt.", .sep = " ")
}

# Testen:
# r <- bereken_resultaat()
# plot_resultaat(r)
#<<<<<<< HEAD
# beschrijf_resultaat(r)
#=======
# beschrijf_resultaat(r)
#>>>>>>> 1c48aa96aef607962363ce11c87c817d85d81187
