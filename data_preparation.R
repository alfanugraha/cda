fileSys <- read.table("data/sistem.csv", header = TRUE, sep=",")
fileOrg <- read.table("data/organisasi.csv", header = TRUE, sep=",")
fileInd <- read.table("data/individu.csv", header = TRUE, sep=",")

saveRDS(fileSys, "fileSys")
saveRDS(fileOrg, "fileOrg")
saveRDS(fileInd, "fileInd")
