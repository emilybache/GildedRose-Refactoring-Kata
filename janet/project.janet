(declare-project
  :name "gilded-rose"
  :description "A program for updating inventory at the Gilded Rose"
  :dependencies ["https://github.com/ianthehenry/judge.git"])
    
(task "test" [] (shell "jpm_tree/bin/judge"))
