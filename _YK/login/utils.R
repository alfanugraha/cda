##-- Encrypting password ----
# digest(user$password, algo = "md5", serialize = F)
# user$encrypt<-str_rev(sapply(user$password, digest, algo="md5"))
str_rev <- function(x) sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")
password_verification <- function(hash, password){
  hash_encrypt <- str_rev(sapply(password, digest, algo="md5"))
  ifelse(hash==hash_encrypt, return(TRUE), return(FALSE))
}
