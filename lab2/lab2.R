dados <- read.table("atividade-maquinas-dsc.txt", header=TRUE)
dados$laboratorio <- gsub("lccpeer@xmpp.ourgrid.org", "LCC1", dados$laboratorio)
dados$laboratorio <- gsub("peer-lcc2@xmpp.ourgrid.org", "LCC2", dados$laboratorio)
dados$laboratorio <- gsub("gaita@gaita.gmf.ufcg.edu.br", "SPLab", dados$laboratorio)
dados$laboratorio <- gsub("peer-lsd@xmpp.ourgrid.org", "LSD", dados$laboratorio)
table(dados$laboratorio)



