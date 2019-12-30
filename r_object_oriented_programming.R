kSimPersonVector <- vector()
setClass("SimPerson",
         slots = c(SSN = "integer", name = "character", age = "integer") )
setClass(
  "Student",
  slots = c(
    id = "integer",
    grade = "character",
    GPA = "numeric",
    major = "character"
  ),
  contains = "SimPerson"
)
setGeneric("AddIndividual", function(object, obj.vector) {
  standardGeneric("AddIndividual")
})
setGeneric("PrintSSN", function(object) {
  standardGeneric("PrintSSN")
})
setGeneric("PrintName", function(object) {
  standardGeneric("PrintName")
})
setGeneric("PrintAge", function(object) {
  standardGeneric("PrintAge")
})
setGeneric("PrintGrade", function(object) {
  standardGeneric("PrintGrade")
})
setGeneric("PrintGPA", function(object) {
  standardGeneric("PrintGPA")
})
setGeneric("PrintId", function(object) {
  standardGeneric("PrintId")
})
setGeneric("PrintMajor", function(object) {
  standardGeneric("PrintMajor")
})
setGeneric("GetSSN", function(object) {
  standardGeneric("GetSSN")
})
setGeneric("GetName", function(object) {
  standardGeneric("GetName")
})
setGeneric("GetAge", function(object) {
  standardGeneric("GetAge")
})
setGeneric("GetGrade", function(object) {
  standardGeneric("GetGrade")
})
setGeneric("GetGPA", function(object) {
  standardGeneric("GetGPA")
})
setGeneric("GetId", function(object) {
  standardGeneric("GetId")
})
setGeneric("GetMajor", function(object) {
  standardGeneric("GetMajor")
})
setGeneric("SetAge", function(object, x) {
  standardGeneric("SetAge")
})
setGeneric("SetGPA", function(object, x) {
  standardGeneric("SetGPA")
})
setGeneric("SetGrade", function(object, x) {
  standardGeneric("SetGrade")
})
setGeneric("SetMajor", function(object, x) {
  standardGeneric("SetMajor")
})
setMethod("AddIndividual", "SimPerson", function(object, obj.vector) {
  append(obj.vector, object, after = length(obj.vector))
  return(obj.vector)
})
setMethod("AddIndividual", "Student", function(object, obj.vector) {
  append(obj.vector, object, after = length(obj.vector))
  return(obj.vector)
})
setMethod("PrintSSN", "SimPerson", function(object) {
  cat(object@SSN, "\n")
})
setMethod("PrintName", "SimPerson", function(object) {
  cat(object@name, "\n")
})
setMethod("PrintAge", "SimPerson" , function(object) {
  cat(object@age, "\n")
})
setMethod("PrintId", "Student", function(object) {
  cat(object@id, "\n")
})
setMethod("PrintGrade", "Student", function(object) {
  cat(object@grade, "\n")
})
setMethod("PrintMajor" , "Student" , function(object) {
  cat(object@major, "\n")
})
setMethod("PrintGPA" , "Student" , function(object) {
  cat(object@GPA, "\n")
})
setMethod("GetSSN" , "SimPerson" , function(object) {
  return(object@SSN)
})
setMethod("GetName" , "SimPerson" , function(object) {
  return(object@name)
})
setMethod("GetAge" , "SimPerson" , function(object) {
  return(object@age)
})
setMethod("GetId" , "Student" , function(object) {
  return(object@id)
})
setMethod("GetGPA" , "Student" , function(object) {
  return(object@GPA)
})
setMethod("GetGrade", "Student" , function(object) {
  return(object@grade)
})

setMethod("GetMajor", "Student", function(object) {
  return(object@major)
})
setMethod("show", "SimPerson", function(object) {
  cat("name:", object@name, "\n age:", object@age, "\n")
})
setMethod("show", "Student", function(object) {
  cat(
    "name:",
    object@name,
    "\n age:",
    object@age,
    "\n grade:",
    object@grade,
    "\n major:",
    object@major,
    "\n gpa:",
    object@GPA,
    "\n"
  )
})
setMethod("SetAge", "SimPerson", function(object, x) {
  object@age <- x
})
setMethod("SetGPA", "Student", function(object, x) {
  object@GPA <- x
  return(object)
})
setMethod("SetGrade", "Student" , function(object, x) {
  object@grade <- x
  return(object)
})
setMethod("SetMajor", "Student", function(object, x) {
  object@major <- x
  return(object)
})
student.one <-
  new(
    "Student",
    grade = "Senior",
    GPA = 4.0,
    major = "Computer Science",
    name = "John",
    age = as.integer(21)
  )
student.two <-
  new(
    "Student",
    grade = "Junior",
    GPA = 4.0,
    major = "Biology",
    name = "Niya",
    age = as.integer(20)
  )

#overwritting a students gpa to update using a vector to store students
student.vector <- c(student.one, student.two)
show(student.vector)
student.vector[[1]] <- SetGPA(student.vector[[1]], 3.5)
show(student.vector)

ReadSSN <- function()
{ 
  n <- readline(prompt="Enter person ssn: ")
  n <- as.integer(n)
  if (is.na(n)){
    n <- readinteger()
  }
  return(n)
}
ReadName <- function()
{ 
  n <- readline(prompt="Enter person name: ")
  n <- as.character(n)
  if (is.na(n)){
    n <- readChar()
  }
  return(n)
}
ReadAge<- function()
{ 
  n <- readline(prompt="Enter person age: ")
  n <- as.integer(n)
  if (is.na(n)){
    n <- readinteger()
  }
  return(n)
}

PromptAgain <- function(vec, obj) {
  vec <- append(vec, obj)
  n <- readline(prompt="Do you want to add another person? (0 = no/ 1 = yes): ")
  n <- as.character(n)
  if (is.na(n)){
    n <- readChar()
  }
  if(n == 1) {
    PersonPrompt(vec)
  }else if(n == 0) {
    
    print("Done.")
    return(vec)
  }
}

PersonPrompt <- function(vec) {
  ssn <- NULL
  name <- NULL
  age <- NULL
  print(ssn<-ReadSSN())
  print(name<-ReadName())
  print(age<-ReadAge())
  test.SimPerson <- new("SimPerson", SSN = ssn, name = name, age = age)
  #show(test.SimPerson)
  #append(kSimPersonVector, test.SimPerson, after = length(kSimPersonVector))
  final.vector = PromptAgain(vec, test.SimPerson)
  return(show(final.vector))
}



PersonPrompt(kSimPersonVector)

