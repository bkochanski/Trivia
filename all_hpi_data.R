bdl::search_subjects("nierucho")
bdl::get_subjects(parentId = "K48")

subj1<-bdl::get_subjects(parentId = "G596")
subj1r<-subj1[!grepl("kwartalne", subj1$name,fixed=TRUE),]
subj1rr<-subj1r[,c("id", "parentId", "name")]
names(subj1rr)<-c("subjectId", "parentId", "subjectName")
subj2<-bdl::get_subjects(parentId = "G597")
subj2r<-subj2[!grepl("kwartalne", subj2$name,fixed=TRUE),]
subj2rr<-subj2r[,c("id", "parentId", "name")]
names(subj2rr)<-c("subjectId", "parentId", "subjectName")

subjrr<-rbind(subj1rr, subj2rr)

bdl::get_variables("P3783")

library(purrr)
df<-map_dfr(c(subj1r$id, subj2r$id), function(x){bdl::get_variables(x)})

df2<-merge(df, subjrr)

powiaty<-bdl::get_units(level="5")

v1<-bdl::get_data_by_variable(df2$id[1], unitLevel = "5", year = 2014:2021)
test1<-bdl::get_data_by_unit(unitId = powiaty$id[1], varId =  df2$id[151:200], year = 2014:2021)
