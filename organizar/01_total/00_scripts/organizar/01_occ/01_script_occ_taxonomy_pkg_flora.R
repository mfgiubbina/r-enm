devtools::install_github("gustavobio/flora")
library(flora)

flora::get.taxa("Araucaria angustifolia")
flora::get.taxa("Cereus albicaulis", 
                states = TRUE, 
                life.form = TRUE, 
                habitat = TRUE, 
                vernacular = TRUE, 
                establishment = TRUE)

flora::suggest.names("Cereus albicaulis")
flora::remove.authors("Araucaria angustifolia (Bertol.) Kuntze")
flora::lower.taxa("Cereus albicaulis")
flora::vernacular("Araucária", exact = TRUE)

fixCase("myrcia lingua")
trim("Myrcia    lingua   ")
standardize.names("Myrcia sp01")
standardize.names("Myrcia sp2")
standardize.names("Myrcia sp.3")

web.flora()
