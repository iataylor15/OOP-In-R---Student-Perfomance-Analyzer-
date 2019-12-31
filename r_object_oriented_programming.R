#This program is a prototype (R oop experimenting) 
#So far, it uses linear regression to analyze the relationship between age and gpa 
#(user inputed values)
#Author: Isaac Taylor
#Updated: 12/30/2019
#vector to store created persons and students
persons.vector <- vector()
#creation of SimPerson  class
setClass("SimPerson",
         slots = c(SSN = "integer", name = "character", age = "integer"))
#creation of Student class
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
#creation of generic funtions that can be customized by specific classes --------
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
#custimization of generic methods to fit needs of specific class------------
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
#function to read ssn
ReadSSN <- function()
{
  #take valid ssn number
  out <- tryCatch({
    n <- readline(prompt = "Enter persons ssn: ")
    n <- as.integer(n)
    if (is.na(n) || as.integer(nchar(paste0(n))) != 9 || n < 0) {
      print("Invalid SSN - Good ex. 123456789")
      n <- ReadSSN()
    }
    return(n)
  }, error = function(e) {
    print("Invalid SSN")
    ReadSSN()
  })
}
#function to read student id
ReadId <- function()
{
  #take valid id
  out <- tryCatch({
    n <- readline(prompt = "Enter students id: ")
    n <- as.integer(n)
    if (is.na(n) || as.integer(nchar(paste0(n))) != 9 || n < 0) {
      print("Invalid Id - Good ex. 123456789")
      n <- ReadId()
    }
    return(n)
  }, error = function(e) {
    print("Invalid Id")
    ReadId()
  })
}

#function to read name
ReadName <- function()
{
  n <- readline(prompt = "Enter persons name: ")
  n <- as.character(n)
  if (is.na(n)) {
    n <- ReadName()
  }
  return(n)
}
#function to read major
ReadMajor <- function()
{
  n <- readline(prompt = "Enter major: ")
  n <- as.character(n)
  if (is.na(n)) {
    n <- ReadMajor()
  }
  return(n)
}

#function to read grade
ReadGrade <- function()
{
  n <- readline(prompt = "Enter students grade level: ")
  n <- as.character(n)
  if (is.na(n)) {
    n <- ReadGrade()
  }
  return(n)
}

# funtion to read gpa
ReadGPA <- function()
{
  out <- tryCatch({
    n <- readline(prompt = "Enter GPA: ")
    n <- as.numeric(n)
    if (is.na(n) || n < 0 || n > 4) {
      print("Invalid GPA")
      n <- ReadGPA()
    }
    return(n)
  },
  error = function(e) {
    print("GPA")
    ReadGPA()
  })
}

# funtion to read age
ReadAge <- function()
{
  out <- tryCatch({
    n <- readline(prompt = "Enter person age: ")
    n <- as.integer(n)
    if (is.na(n)) {
      n <- ReadAge()
    }
    return(n)
  },
  error = function(e) {
    print("Invalid Age")
    ReadAge()
  })
}

# function for object type creation decision
ReadDecision <- function(vec)
{
  out <- tryCatch({
    n <-
      readline(prompt = "Enter 0 to create a person object, 1 to create a student object, or 2 to quit: ")
    n <- as.integer(n)
    if (is.na(n) || (n != 0 && n != 1 && n != 2)) {
      print("Invalid Choice")
      ReadDecision(vec)
    } else if (n == 0) {
      vec <- PersonPrompt(vec)
    } else if (n == 1) {
      vec <- StudentPrompt(vec)
    } else if (n == 2) {
      print("Done")
    }
    return(vec)
  },
  error = function(e) {
    print(e)
    print("Invalid Choice")
    ReadDecision(vec)
  })
}

#funtion that adds a user created SimPerson or Student to a vector, then prompts to
#add another instance to a vector or not
PromptAgain <- function(vec, obj) {
  vec <- append(vec, obj)
  ReadDecision(vec)
}

#function that prompts user to create a Student
StudentPrompt <- function(vec) {
  ssn <- NULL
  name <- NULL
  age <- NULL
  id <- NULL
  grade <- NULL
  gpa <- NULL
  major <- NULL
  print(ssn <- ReadSSN())
  print(name <- ReadName())
  print(age <- ReadAge())
  print(id <- ReadId())
  print(grade <- ReadGrade())
  print(gpa <- ReadGPA())
  print(major <- ReadMajor())
  test.Student <-
    new(
      "Student",
      SSN = ssn,
      name = name,
      age = age,
      id = id,
      grade = grade,
      GPA = gpa,
      major = major
    )
  final.vector = PromptAgain(vec, test.Student)
  return(final.vector)
}
#function that prompts user to create a SimPerson
PersonPrompt <- function(vec) {
  ssn <- NULL
  name <- NULL
  age <- NULL
  print(ssn <- ReadSSN())
  print(name <- ReadName())
  print(age <- ReadAge())
  test.SimPerson <-
    new("SimPerson",
        SSN = ssn,
        name = name,
        age = age)
  final.vector = PromptAgain(vec, test.SimPerson)
  return(final.vector)
}
# function that creates plot of age and gpa when given a vector of SimPersons
PlotAgeAndGPA <- function(vect) {
  age <- vector(mode = "numeric")
  gpa <- vector(mode = "numeric")
  for (obj in vect) {
    if (is(obj, "Student")) {
     # print(GetAge(obj))
      #print(GetGPA(obj))
      age <- append(age, GetAge(obj))
      gpa <- append(gpa, GetGPA(obj))
    }
  }
  data.vals <- data.frame(cbind(age, gpa))
  show(data.vals)
  # min length of points to form reg line is 3 so sum should be 6
  if (length(age) == length(gpa) && (length(age)+length(gpa)) >= 6) {
    plot(
      age,
      gpa,
      pch = 16,
      cex = 1.3,
      col = "green",
      main = "AGE PLOTTED AGAINST GPA",
      xlab = "AGE",
      ylab = "GPA"
    )
    data.info <- lm(gpa ~ age, data = data.vals)  
    abline(data.info )
    summary(data.info)
  }
}


#examples of Student instance creation ----------
# student.one <-
#   new(
#     "Student",
#     grade = "Senior",
#     GPA = 3.7,
#     major = "Computer Science",
#     name = "John",
#     age = as.integer(21)
#   )
# student.two <-
#   new(
#     "Student",
#     grade = "Junior",
#     GPA = 4.0,
#     major = "Biology",
#     name = "Niya",
#     age = as.integer(20)
#   )
# student.three <- new(
#   "Student",
#   grade = "Sophomore",
#   GPA = 3.0,
#   major = "Biology",
#   name = "Niya",
#   age = as.integer(32)
# )
# student.four <- new(
#   "Student",
#   grade = "Sophomore",
#   GPA = 2.6,
#   major = "Chemistry",
#   name = "Niya",
#   age = as.integer(19)
# )
# 
# persons.vector <- c(student.one, student.two, student.three, student.four)
#overwritting a students gpa to update using a vector to store students
#student.vector <- c(student.one, student.two)
#show(student.vector)
#student.vector[[1]] <- SetGPA(student.vector[[1]], 3.5)
#show(student.vector)


persons.vector <- ReadDecision(persons.vector)
show(persons.vector)
PlotAgeAndGPA(persons.vector)
