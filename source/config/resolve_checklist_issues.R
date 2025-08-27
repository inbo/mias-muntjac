# add words to custom dictionary
x <- checklist::check_spelling()
checklist::custom_dictionary(x)
checklist::write_checklist(x) #needed? check changes in checklist.yml
#
# change language for specific files (interactive)
if (FALSE) {
  #x$checklist::set_exceptions()
  checklist::write_checklist(x)
}
#
# solve CITATION error: invalid ORCID
# changed read me manually
#
# add exceptions for notes and warnings (interactive)
x <- checklist::check_project()
checklist::write.checklist(x)
