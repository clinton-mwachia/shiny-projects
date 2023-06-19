box <- function(id, title, value=0, class="", style=""){
  div(
    class="box",
    div(style="background-color:#75a3a3",
      p(title, class=class),
    ),
    div(
      p(value, class="value"),
    )
  )
}

ValueBoxes <- function(value=0, value2=0,value3=0, value4=0){
  div(
    div(
      box(title = "Total Loan", value = value, class="text"),
      box(title = "Total Members" , value=value2, class="text-2"),
      box(title = "Total Loan Paid" , value=value3, class="text-4"),
      box(title = "Total Loan Default" , value=value4, class="text-3")
    )
  )
}
nchar(1000)
# format in thousands
Thousand <- function(number){
  if(nchar(number) == 4){
    paste0(format(round(number/1000, 1)),"K")
  } else {
    paste0(format(round(number/1000000, 1)),"M")
  }
}
