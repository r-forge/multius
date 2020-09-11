\name{firm_001}
\alias{firm_001}
\docType{data}
\title{
Adacta dataset
}
\description{
The dataset contains binary networks. The units are employed in Adacta organization. The data were collected in 2004, 2006 and 2007.
}
\usage{data("firm_001")}
\format{
The list contains the following elements:
  \describe{
    \item{\code{nasveti (_06, _07) (binary network)}}{Na koga iz podjetja se obrnete, ko potrebujete nasvet ali informacijo v povezavi z delovnimi problemi?}
    \item{\code{ideje (_04, _07) (binary network)}}{Pri vasem delu se vam verjetno veckrat porajajo nove ideje. V pogovoru s katerim izmed vasih sodelavcev se vam obicajno porodijo nove ideje, ki bi jih lahko uporabili pri vasem delu?}
        \item{\code{testiranje (_04, _07) (binary network)}}{Ko se vam porodi nova ideja, boste poskusali preveriti, ce je ta ideja dobra. Komu od vasih sodelavcev obicajno zaupate svoj idejo, da jo lahko oceni kot (ne)ustrezno oz. kdo vas podpira pri uveljavljanju te ideje?}
    \item{\code{ucitelji (_04,  _06, _07) (binary network)}}{Kdo so ljudje v vasi organizaciji, od katerih se najvec ucite?}
    \item{\code{ucenci (_06) (binary network)}}{Kdo so ljudje v vasi organizaciji, ki se najpogosteje ucijo od vas?}
    \item{\code{druzenje (_06, _07) (binary network)}}{Koga iz podjetja smatrate za kolega t.j. cloveka, s katerim se druzite tudi v prostem casu, hodite skupaj na kosilo, pijaco ipd.?}

        \item{\code{spol (_04, _06, _07) (vector)}}{Gender (1=male, 2=female)}
        \item{\code{oddelek (_04, _06, _07) (vector)}}{Department (0=common services, 1=BU Navision, 2=BU Industry solutions, 3=BU Banking solutions, 4=directorate, 6=NA)}
        \item{\code{staz (_04, _06, _07) (vector)}}{Number of months of employment}
        \item{\code{hierpos (_04, _06, _07) (vector)}}{Hierarchical position in the company (1=higher managment, 2=middle managment, 3=project managment, 4=line managment, 5=contractors (izvajalci))}
        \item{\code{lokacija (_04, _06, _07) (vector)}}{Location (1=Ljubljana, 2=Zagreb, 3=Beograd, 0=NA)}
  }
}


\details{
The rownames and columnames in the networks corresponding to the respondents' IDs. They are mainly consistent in all networks and all time periods.
}


\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}

\keyword{datasets}
