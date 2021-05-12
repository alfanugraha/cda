library(RPostgreSQL)
library(DBI)
# library(RMariaDB)

driver <- dbDriver('PostgreSQL')
DB <- dbConnect(
  driver, dbname='moodle2', host='pepstaging.duckdns.org', port='5432',
  user='moodleaksara', password='moodleaksaradbpassword'
)

aksara_tbl <- data.frame(dbListTables(DB))

# CONCAT(u.firstname, ' ', u.lastname) AS "Display Name", 
# FROM_UNIXTIME(gg.timemodified) AS TIME
q <- paste0('SELECT u.firstname AS "First" , u.lastname AS "Last", c.fullname AS "Course", cc.name AS "Category",
 
CASE 
  WHEN gi.itemtype = \'course\'
   THEN c.fullname 
  ELSE gi.itemname
END AS "Item Name",
 
ROUND(gg.finalgrade,2) AS Grade

 
FROM mdl_course AS c
JOIN mdl_context AS ctx ON c.id = ctx.instanceid
JOIN mdl_role_assignments AS ra ON ra.contextid = ctx.id
JOIN mdl_user AS u ON u.id = ra.userid
JOIN mdl_grade_grades AS gg ON gg.userid = u.id
JOIN mdl_grade_items AS gi ON gi.id = gg.itemid
JOIN mdl_course_categories AS cc ON cc.id = c.category
 
WHERE  gi.courseid = c.id 
ORDER BY lastname
')

# dbSendQuery(DB, q)
result <- dbGetQuery(DB, q)


#*query nilai:
  q <- paste0('  SELECT u.firstname, u.lastname, u.email, gi.itemname as quiz, gg.finalgrade, gi.grademax, gi.grademin, gg.timecreated, gg.timemodified
FROM mdl_quiz AS q
JOIN (
  SELECT cm.instance 
  FROM mdl_course_sections as cs
  JOIN mdl_course AS c ON c.id = cs.course
  JOIN mdl_course_modules AS cm ON cm.section = cs.id
  JOIN mdl_modules AS m ON m.id = cm.module
  WHERE c.shortname LIKE \'%\' AND (cs.name LIKE \'%\' OR cs.name IS NULL) AND m.name = \'quiz\'
) as cmlist ON cmlist.instance = q.id
JOIN mdl_grade_items AS gi ON gi.iteminstance = cmlist.instance AND gi.itemmodule = \'quiz\' AND gi.itemtype = \'mod\'
JOIN mdl_grade_grades AS gg ON gg.itemid = gi.id
JOIN mdl_user AS u ON u.id = gg.userid
WHERE u.username LIKE \'%\'')
#*filter dari kelas, nama section, user

  result <- dbGetQuery(DB, q)
  
#query section:
  q1 <- paste0('  SELECT cs.id, cs.name, c.fullname as coursename
FROM mdl_course AS c
JOIN mdl_course_sections AS cs ON cs.course = c.id
WHERE c.shortname LIKE \'%\' AND cs.visible = 1
ORDER BY section')
#*filter dari kelas

  result1 <- dbGetQuery(DB, q1)

q2 <- paste0('SELECT * FROM mdl_course_list_cdna')

result2 <- dbGetQuery(DB, q2)


kuis_prk <- subset(result, select=c(firstname, lastname, email, quiz, finalgrade))


rs <- dbSendStatement(
  DB,
  "CREATE TABLE public.mdl_course_list_cdna (
    id bigint NOT NULL,
    username character varying(300),
    sectionid character varying(300)
);

ALTER TABLE ONLY public.mdl_course_list_cdna
    ADD CONSTRAINT mdl_courlistcdna_id_pk PRIMARY KEY (id);

CREATE UNIQUE INDEX mdl_courlistcdna_id2_uix ON public.mdl_course_list_cdna USING btree (id);"
)


q3<-paste0('SELECT u.firstname, u.lastname, u.email, cmlist.sectionname,cmlist.sectionid, cmlist.courseid, gi.itemname as quizname, gg.finalgrade, gi.grademax, gi.grademin, gg.timecreated, gg.timemodified
FROM mdl_quiz AS q
JOIN (
SELECT cm.instance, cs.name as sectionname, cs.id as sectionid, c.id as courseid
FROM mdl_course_sections as cs
JOIN mdl_course AS c ON c.id = cs.course
JOIN mdl_course_modules AS cm ON cm.section = cs.id
JOIN mdl_modules AS m ON m.id = cm.module
WHERE c.shortname LIKE \'%\' AND (cs.name LIKE \'%\' OR cs.name IS NULL) AND m.name = \'quiz\'
) as cmlist ON cmlist.instance = q.id
JOIN mdl_grade_items AS gi ON gi.iteminstance = cmlist.instance AND gi.itemmodule = \'quiz\' AND gi.itemtype = \'mod\'
JOIN mdl_grade_grades AS gg ON gg.itemid = gi.id
JOIN mdl_user AS u ON u.id = gg.userid
WHERE u.username LIKE \'%\'')

result3 <- dbGetQuery(DB, q3)

kuis_prk <- subset(result3, select=c(firstname, lastname, email, sectionname, sectionid, courseid, quizname, finalgrade))


q4<-paste0('SELECT COUNT(1) FROM mdl_course_list_cdna WHERE username = \'alfa.nugraha@gmail.com\'')
qrs <- dbSendStatement(DB, q4)
res<-dbGetRowsAffected(qrs)
dbClearResult(qrs)

yes<-paste(tblInsertMoodle$sectionid, collapse=",")

q5<-paste0('INSERT INTO mdl_course_list_cdna(id, username, sectionid) VALUES (\'', email, '\',\'', yes,'\')')

dbExecute(DB, q5)
rs5 <- dbSendStatement(DB, q5)

no <- '1,2,3,4,5'
q6<-paste0("UPDATE mdl_course_list_cdna SET sectionid = '", no, "' WHERE username = '", email, "'")


KeyPlusOne <- sum(SQLCommand('SELECT count(*) FROM mdl_course_list_cdna'), 1)
NewRecord <- data.frame(id=KeyPlusOne, username=email, sectionid=yes)
SQLWriteValues(NewRecord, 'mdl_course_list_cdna')


SQLCommand('TRUNCATE TABLE mdl_course_list_cdna;')


a<-dbReadTable(con, 'mdl_course_list_cdna')


driver <- dbDriver('PostgreSQL')

DB <- dbConnect(
  driver, dbname='ProjectDB', host='projectdb.agroforestri.id', port='5432',
  user='projectdb', password='F$8j7kb8'
)
