# Applied Statistics Course Project: Journey

## RStudio project evironment

### How to setup the environment 
- Open RStudio
- File > New Project > Version Control > Git
- Paste https://kinder3141@bitbucket.org/AppStat_Project/appstat_project.git

### Directories structure 

#### local_*
Every folder which name stars with "local_" is ***NOT*** tracked by Git. It's important to maintain huge files in local folders. (e.g. the dataset is stored in local_data, big size documentation in local_doc, etc).

#### doc
Shared Documentation: articles, pitch presentation,...

#### script
Here goes every .R script

#### plot
save here interesting plots

#### other files/folders
- .gitignore:   git file reporting which files/folder not to track
- appstat_project.Rproj:    RStudio project file

### How to organize the workflow 

#### Branches:
- `master`: master branch not to be modified. This contains ONLY very important commits and it's update only by one person.
- `develop`: development branch. It's the common working branch. Everyone work on this branch locally committing, not caring about others' work. When satisfied of her work and everything is locally committed:
    - `Pull` the `origin/develop` branch
    - if there are conflicts, solve manually every conflict
    - `Commit` the conflict resolution locally
    - `Push` the final version on `origin/develop`
