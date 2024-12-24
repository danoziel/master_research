library(readxl)
library(dplyr)
library(stringr)
library(writexl)

# Load the Excel file
 df<- read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/course_report_66f9367b664424f65df28219_21102024.xlsx")
library(readxl)
course_report_demo02 <- read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/course_report_demo02.xlsx", 
                                   sheet = "course_report_66af72fec8a6bd2b8")

names(course_report_demo02)

course_report_demo02 %>% group_by(Mobile) %>%  
  summarise(Mean=mean(Minutes_in_session),n=n())%>%  
  summarise(mean(Mean),n=mean(n))



# [1] "Name"                             
# [2] "Mobile"                           
# [3] "session_date"                     
# [4] "session_content"                  
# [5] "Minutes_in_session"               
# [6] "msg_in_session"                   
# [7] "total_msg_user"                   
total_msg_user_noContent 
total_msg_user_noEnglish

msg_user

words_in_session
words_user
wordsUnique_user

"Average_words_in_one_user_message"
"vocabulary"                       
"valid_session"                    





# Function to count English words only, excluding "assistant", "user", and "(no content)"
count_english_words <- function(text) {
  # Remove signs, numbers, emojis, Hebrew, Arabic, "assistant", "user", and "(no content)"
  clean_text <- str_remove_all(text, "(user|assistant|no content)")  # Remove "user", "assistant", and "(no content)"
  clean_text <- str_remove_all(clean_text, "[^A-Za-z\\s]")  # Keep only English alphabet and spaces
  
  # Tokenize the words
  words <- str_split(clean_text, "\\s+") %>% unlist()
  
  # Filter out empty tokens
  words <- words[words != ""]
  
  return(length(words))
}

# Function to count only English words in user messages
count_english_user_words <- function(text) {
  # Extract only user messages (assuming 'user:' indicates user messages)
  user_messages <- str_extract_all(text, "user:.*") %>% unlist() %>% paste(collapse = " ")
  
  # Remove signs, numbers, emojis, Hebrew, Arabic, "assistant", "user", and "(no content)"
  clean_text <- str_remove_all(user_messages, "(user|assistant|no content)")  # Remove "user", "assistant", and "(no content)"
  clean_text <- str_remove_all(clean_text, "[^A-Za-z\\s]")  # Keep only English alphabet and spaces
  
  # Tokenize the words
  words <- str_split(clean_text, "\\s+") %>% unlist()
  
  # Filter out empty tokens
  words <- words[words != ""]
  
  return(length(words))
}



df_sky1 <- course_report_demo02 %>% 
  select(Mobile, session_date, session_content, 
         msg_in_session,total_msg_user,Spelling_errors_in_session,voc) %>% 
  rename(id=Mobile) %>% mutate(id=as.character(id))


# Apply the functions to the session content column
df_sky2 <- df_sky1 %>%
  mutate(words_session = sapply(session_content, count_english_words),
         words_user = sapply(session_content, count_english_user_words))


################################################
data = df_sky2[325,3]
data <- data.frame(
  id = c("972533312581", "972508581717"),
  session_date = c("2024-10-28T16:40:49.908Z", "2024-09-29T06:39:33.797Z"),
  session_content = c(
    "10:01 user: (no content)\r\n11:04 user: I like?\r\n10:35 assistant: Consistency is key! Continue your English session and keep advancing.",
    "11:04 user: I'm very tire today so let's talk afternoon please\r\n11:04 assistant: I under\r\n11:04 assistant: I understand. I'll be here whenever you're ready to chat in the afternoon! 😊\r\n11:04 user: I Don't forget to continue morning morning morning  like llike good  . "
  ),
  stringsAsFactors = FALSE
)

library(dplyr)

# Original data
data <- data.frame(
  id = c("972533312581", "972508581717"),
  session_date = c("2024-10-28T16:40:49.908Z", "2024-09-29T06:39:33.797Z"),
  session_content = c(
    "10:01 user: (no content)\r\n11:04 user: היי מה קורה?\r\n10:35 assistant: Consistency is key! Continue your English session and keep advancing.",
    "11:04 user: I'm very tire today so let's talk afternoon please\r\n11:04 assistant: I understand I'll be here whenever you're ready to chat in the afternoon! 😊\r\n11:04 user: I Don't forget to continue morning morning morning like like good שלום . "
  ),
  stringsAsFactors = FALSE
)

# Step 1: Clean user_msgs and calculate word counts
cleaned_data <- data %>%
  mutate(user_msgs = gsub("assistant:.*?user:", "user:", session_content)) %>% # Remove text between "assistant:" and "user:"
  mutate(user_msgs = gsub("assistant:.*", "", user_msgs)) %>%  # Remove text after "assistant:"
  mutate(user_msgs = gsub("\\b\\d{2}:\\d{2}\\b", "", user_msgs)) %>% # Remove hours (hh:mm format)
  mutate(user_msgs = gsub("user: \\(no content\\)", "", user_msgs)) %>% # Remove "user: (no content)"
  mutate(user_msgs = gsub("[\u0590-\u05FF]", "", user_msgs)) %>% # Remove "user: (no content)"
  mutate(user_msgs = gsub("[?!./]", "", user_msgs)) %>% # Remove punctuation (?!/.)
  
  mutate(user_msgs = gsub("\\s+", " ", user_msgs)) %>% # Clean extra spaces
  mutate(user_msgs = trimws(user_msgs)) %>% # Remove leading/trailing spaces
  mutate(user_msgs = gsub("user:", "", user_msgs)) %>% # Remove "user:"
  mutate(
    word_count = sapply(strsplit(user_msgs, "\\s+"), function(x) {
      sum(grepl("^[a-zA-Z']+$", x)) # Match all English words (including contractions)
    }),
    user_msgsUniqe = sapply(user_msgs, function(msg) {
      unique_words <- unique(unlist(strsplit(msg, "\\s+"))) # Split and get unique words
      sum(grepl("^[a-zA-Z']+$", unique_words)) # Count only English unique words
    })
  )

# View the result
print(cleaned_data)













# Save the updated data as an Excel file
write_xlsx(df, "updated_course_report.xlsx")



















