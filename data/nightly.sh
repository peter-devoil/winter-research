#!/bin/bash
cd /home/data/access-s/met
Rscript getIt.bomNC.R
Rscript apsimFileGenerator.R
cd cache/bom
/usr/local/bin/Apsim.exe *.apsim
cd /home/data/access-s/met
Rscript simCollector.R

scp  -i /home/uqpdevo1/.ssh/apsim-poama-nectar.pem cache/bom/ddf.*.RData apsimpoama:/srv/shiny-server/lwells/
