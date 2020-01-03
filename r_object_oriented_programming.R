#TObject Oriented Programming in R
#It uses linear regression to analyze the relationship between age and gpa of students
#(user inputed values)
#Author: Isaac Taylor
#Updated: 01/02/2019
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
    n <- readline(prompt = "Enter students id (4 digit): ")
    n <- as.integer(n)
    if (is.na(n) || as.integer(nchar(paste0(n))) != 4 || n < 0) {
      print("Invalid Id - Good ex. 1234")
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
ReadDecision <- function(vect)
{
  out <- tryCatch({
    n <-
      readline(prompt = "Enter 1 to create a new student, 2 to view a students info, 3 to update a students info, 4 to remove a student,  or 0 to quit: ")
    n <- as.integer(n)
    vect <- mmergesort(vect)
    if (is.na(n) ||
        (n != 0 && n != 1 && n != 2 && n != 3 && n != 4)) {
      print("Invalid Choice")
      ReadDecision(vect)
    } else if (n == 1) {
      vect <- StudentPrompt(vect)
      ReadDecision(vect)
    } else if (n == 2) {
      ViewStudent(vect)
      ReadDecision(vect)
    } else if (n == 3) {
      vect <- UpdateStudent(vect)
      ReadDecision(vect)
    } else if (n == 4) {
      vect <- RemoveStudent(vect)
      ReadDecision(vect)
    } else if (n == 0) {
      print("Done")
    }
    return(vect)
  },
  error = function(e) {
    print(e)
    print("Invalid Choice")
    ReadDecision(vect)
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
  #print(ssn <- ReadSSN())
  print(name <- ReadName())
  print(age <- ReadAge())
  print(id <- ReadId())
  print(grade <- ReadGrade())
  print(gpa <- ReadGPA())
  print(major <- ReadMajor())
  test.Student <-
    new(
      "Student",
      # SSN = ssn,
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
  if (length(age) == length(gpa) &&
      (length(age) + length(gpa)) >= 6) {
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
    abline(data.info)
    summary(data.info)
  }
}

ViewStudent <- function(vect) {
  val <- ReadId()
  target <- BSearch(vect, val , 1, length(vect))
  if (target > 0) {
    show(vect[target])
    return(target)
  } else {
    print("Student not found")
    return(-1)
  }
}

UpdateStudent <- function(vect) {
  target <- ViewStudent(vect)
  if (target > 0) {
    out <- tryCatch({
      n <-
        readline(prompt = "Which value do you wish to update (age = 1, grade = 2, major  = 3, gpa = 4)?:  ")
      n <- as.integer(n)
      if (is.na(n)) {
        UpdateStudent(vect)
      } else if (n == 1) {
        age <- ReadAge()
        show(vect[target])
        vect[target] <- SetAge((vect[target])[[1]], age)
        
      } else if (n == 2) {
        grade <- ReadGrade()
        vector[target] <- SetGrade((vect[target])[[1]], grade)
      } else if (n == 3) {
        major <- ReadMajor()
        
        vect[target] <- SetMajor((vect[target])[[1]], major)
      } else if (n == 4) {
        gpa <- ReadGPA()
        show(vect[target])
        vect[target] <- SetGPA((vect[target])[[1]], gpa)
      }
      show(vect[target])
      return(vect)
    },
    error = function(e) {
      print(e)
      print("Invalid selection")
      UpdateStudent(vect)
    })
  }
  
}

RemoveStudent <- function(vect) {
  target <- ViewStudent(vect)
  remove <- c(NA)
  if (target > 0) {
    vect[target] <- NA
    vect <- vect[!is.na(vect)]
    cat("Student has succefully been removed.")
    return(vect)
  } else{
    return(vect)
  }
}
#merge function for merging vectors of Students after they are sorted
mmerge <- function(a, b) {
  r <- numeric(length(a) + length(b))
  ai <- 1
  bi <- 1
  j <- 1
  
  for (j in 1:length(r)) {
    if ((ai <= length(a) &&
         IsLess(((a[ai])[[1]]), ((b[bi])[[1]]))) ||
        bi > length(b)) {
      r[j] <- a[ai]
      ai <- ai + 1
    } else {
      r[j] <- b[bi]
      bi <- bi + 1
    }
  }
  r
}
#merge sort function for sorting Students
mmergesort <- function(A) {
  if (length(A) > 1) {
    q <- ceiling(length(A) / 2)
    a <- mmergesort(A[1:q])
    b <- mmergesort(A[(q + 1):length(A)])
    mmerge(a, b)
  } else {
    return(A)
  }
}

#fuction returns true if Student obj's id is less than Student obj.two's id
IsLess <- function(obj, obj.two) {
  if (!is.null(obj) && !is.null(obj.two)) {
    return (as.integer(GetId(obj)) < as.integer(GetId(obj.two)))
  } else{
    return(0 > 1)
    
  }
}
#fuction returns true if id is identical to Student obj.two's id
IsEqual <- function(id, obj.two) {
  if (!is.null(id) && !is.null(obj.two)) {
    return (isTRUE(as.integer(id) == as.integer(GetId(obj.two))))
  } else{
    return(0 > 1)
    
  }
}
#fuction returns true if id is greater than Student obj.two's id
IsGreatearr <- function(id, obj.two) {
  if (!is.null(id) && !is.null(obj.two)) {
    return (isTRUE(as.integer(id) > as.integer(GetId(obj.two))))
  } else{
    return(0 > 1)
    
  }
}
#fuction returns true if id is less than Student obj.two's id
IsLesss <- function(id, obj.two) {
  if (!is.null(id) && !is.null(obj.two)) {
    return (isTRUE(as.integer(id) < as.integer(GetId(obj.two))))
  } else{
    return(0 > 1)
    
  }
}
#binary search for students by id
BSearch <- function(vect, val, low, high) {
  if (length(vect) >= 1) {
    mid <- as.integer((low + high) / 2)
    if ((high-low <= 2)){
        m <-IsEqual(val , (vect[mid])[[1]])  
        l <-IsEqual(val , (vect[low])[[1]])
        h <-IsEqual(val , (vect[high])[[1]]) 
        if(isTRUE(m)){
          return(mid)
        }else if(isTRUE(l)){
          return(low)
        }else if(isTRUE(h)){
          return(high)
        }
      return(-1)
    } else if (IsEqual(val , (vect[mid])[[1]])) {
      return(mid)
    } else if (IsGreatearr(val , (vect[mid])[[1]])) {
      low <- mid+1
      return(BSearch(vect, val, low, high))
    } else if (IsLesss(val , (vect[mid])[[1]])) {
      high <- mid-1
      return(BSearch(vect, val, low, high))
    }
  } else{
    return(-1)
  }
}

#examples of Student instance creation ----------
StudentCreationTest <- function() {
  student.one <-
    new(
      "Student",
      grade = "Senior",
      GPA = 3.7,
      major = "Computer Science",
      name = "John",
      age = as.integer(21),
      id = as.integer(8399)
    )
  student.two <-
    new(
      "Student",
      grade = "Junior",
      GPA = 4.0,
      major = "Biology",
      name = "Chris",
      age = as.integer(20),
      id = as.integer(8389)
    )
  student.three <- new(
    "Student",
    grade = "Sophomore",
    GPA = 3.0,
    major = "Biology",
    name = "Niya",
    age = as.integer(32),
    id = as.integer(7367)
  )
  student.four <- new(
    "Student",
    grade = "Sophomore",
    GPA = 2.6,
    major = "Chemistry",
    name = "Shay",
    age = as.integer(19),
    id = as.integer(3589)
  )
  return(persons.vector <-
           c(student.one, student.two, student.three, student.four))
}

#This function starts the program
RunProgram <- function(){
#additional student data for testing the program 
persons.vector <- StudentCreationTest()
#actual user info
persons.vector <- ReadDecision(persons.vector)
PlotAgeAndGPA(persons.vector)
}

RunProgram()