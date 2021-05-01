library(frbs)
#membuat fungsi keanggotaan variabel input
varinp.mf <- matrix(c(4,0,10,1000,5000,4,1000,5000,6000,6000,
				4,0,10,100,600,4,100,600,700,700),
				nrow = 5, byrow = FALSE)

#membuat matriks sesuai dengan jumlah variabel
num.fvalinput <- matrix(c(2, 2), nrow=1)

#membuat nilai linguitik setiap variabel
varinput.1 <- c("turun", "naik")
varinput.2 <- c("sedikit", "banyak")
names.varinput <- c(varinput.1, varinput.2)

#membuat semesta pembicaraan setiap variabel
range.data <- matrix(c(0,6000, 0, 700, 0, 8000), nrow=2)

#menentukan metode defuzzifikasi
type.defuz <- "COG"

#menentukan fungsi implikasi dan agregasi
type.tnorm <- "MIN"
type.snorm <- "MAX"

#memasukan data yang ingin diprediksi
newdata<- matrix(c(4000,300), nrow= 1, byrow = TRUE)

#memberikan nama untuk setiap varaibel
colnames.var <- c("input1", "input2", "output1")

#menetukan jumlah nilai linguitik variabel output
num.fvaloutput <- matrix(c(2), nrow=1)

#menetukan nilai linguistik untuk variabel output
varoutput.1 <- c("berkurang", "bertambah")
names.varoutput <- c(varoutput.1)

#membuat fungsi keanggotaan variabel output
varout.mf <- matrix(c(4,0,10,2000,7000,4,2000,7000,8000,8000),
                       nrow = 5, byrow = FALSE)

#memilih metode yang digunakan
type.model <- "MAMDANI"

#membuat aturan fuzzy 
rule <- matrix(c("turun","and","banyak","->","berkurang",
                  "turun","and","sedikit", "->", "berkurang", 
                  "naik","and","banyak","->","bertambah",
                  "naik","and","sedikit", "->", "bertambah"), 
                  nrow=4, byrow=TRUE) 

 rule<-rulebase(type.model,rule,func.tsk="NULL") 

#proeses fuzzifikasi
num.varinput<-ncol(num.fvalinput)
MF<-fuzzifier(newdata,num.varinput,num.fvalinput,varinp.mf)

#proses agregasi
miu.rule<-inference(MF,rule,names.varinput,type.tnorm, type.snorm)

#proses defuzzifikasi
range.output<-range.data[,ncol(range.data),drop = FALSE]
result<-defuzzifier(newdata, rule, range.output, names.varoutput,
varout.mf, miu.rule,type.defuz,type.model, func.tsk="NULL")