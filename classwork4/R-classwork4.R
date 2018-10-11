data <- read.table("https://raw.githubusercontent.com/SergeyMirvoda/MD-DA-2018/master/data/gmp.dat")
gmp <- data.frame(data)
gmp$pop <- gmp$gmp / gmp$pcgmp
y0 <- 6611 

estimate.scaling.exponent <- function(a, y0=6611, response=gmp$pcgmp,
                                      predictor = gmp$pop, maximum.iterations=100, deriv.step = 1/100,
                                      step.scale = 1e-12, stopping.deriv = 1/100) {
  mse <- function(a) { mean((response - y0*predictor^a)^2) }
  for (iteration in 1:maximum.iterations) {
    deriv <- (mse(a+deriv.step) - mse(a))/deriv.step
    a <- a - step.scale*deriv
    if (abs(deriv) <= stopping.deriv) { break() }
  }
  fit <- list(a=a,iterations=iteration,
              converged=(iteration < maximum.iterations))
  result <- c(a, y0)
  names(result) <- c("a", "y0")
  return(fit)
}
#Ïğèìåğ âûçîâà ñ íà÷àëüíûì çàí÷åíèåì a
f <- estimate.scaling.exponent(0.15)
print(f) #f = 0.1211533

#Ñ ïîìîøüş ïîëó÷åííîãî êîıôôèöèåíòà ïîñòğîéòå êğèâóş (ôóíêöèÿ curve) çàâèñèìîñòè
curve(y0 * x ^ f$a, xlab = "Íàñåëåíèå", ylab = "ÂÂÏ / ×åëîâåê", from = 1, to = 500)

#Óäàëèòå òî÷êó èç íàáîğà èñõîäíûõ äàííûõ ñëó÷àéíûì îáğàçîì, êàê èçìåíèëîñü ñòàòèñòè÷åñêàÿ îöåíêà êîıôôèöèåíòà a?
set.seed(234567)
gmp <- gmp[-c(sample.int(nrow(gmp), 1))]
fch <- estimate.scaling.exponent(0.15)
f$a - fch$a #0
#Çíà÷åíèå íå èçìåíèëîñü

#Çàïóñòèòå îöåíêó íåñêîëüêî ğàç ñ ğàçíûõ ñòàğòîâûõ òî÷åê. Êàê èçìåíèëîñü çíà÷åíèå a?
f1 <- estimate.scaling.exponent(0.25)
print(f1)#-0.4752706; Çíà÷åíèå óìåíüøèëîñü
f2 <- estimate.scaling.exponent(0)
print(f2)#0.1211533; Çíà÷åíèå íå èçìåíèëîñü
f3 <- estimate.scaling.exponent(-0.2)
print(f3)#-0.1337248; Çíà÷åíèå óìåíüøèëîñü

#Ôóíêöèÿ predict
predict.plm <- function(obj, dt) {
  # Ïğîâåğèì, åñòü ëè íóæíûå íàì êîìïîíåíòû â îáúåêòå
  stopifnot("a" %in% names(obj), "y0" %in% names(obj))
  a <- obj$a
  y0 <- obj$y0
  # Ïğîâåğêà âõîäíûõ äàííûõ
  stopifnot(is.numeric(a),length(a)==1)
  stopifnot(is.numeric(y0),length(y0)==1)
  stopifnot(is.numeric(dt))
  return(y0*dt^a) # Âû÷èñëèì è âûéäåì
}

# Óëó÷øèòå ôóíêöèş èç ïğèìåğà ê ëåêöèè 4.1 (êîä â ôàéëå 4.1.R)
predict.plm <- function(obj, dt) {
  # Ïğîâåğèì, åñòü ëè íóæíûå íàì êîìïîíåíòû â îáúåêòå
  if ( !("a" %in% names(obj)) && !("y0" %in% names(obj)) ) return(NA)
  
  stopifnot("a" %in% names(obj), "y0" %in% names(obj))
  a <- obj$a
  y0 <- obj$y0
  # Ïğîâåğêà âõîäíûõ äàííûõ
  stopifnot(is.numeric(a),length(a)==1)
  stopifnot(is.numeric(y0),length(y0)==1)
  stopifnot(is.numeric(dt))
  return(y0*dt^a) # Âû÷èñëèì è âûéäåì}
}
