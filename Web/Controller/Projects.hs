module Web.Controller.Projects where

import Web.Controller.Prelude
import Web.View.Projects.Index
import Web.View.Projects.New
import Web.View.Projects.Edit
import Web.View.Projects.Show

instance Controller ProjectsController where
    action ProjectsAction = do
        projects <- query @Project |> fetch
        printOnes
        render IndexView { .. }

    action NewProjectAction = do
        let project = newRecord
        render NewView { .. }

    action ShowProjectAction { projectId } = do
        project <- fetch projectId
        render ShowView { .. }

    action EditProjectAction { projectId } = do
        project <- fetch projectId
        render EditView { .. }

    action UpdateProjectAction { projectId } = do
        project <- fetch projectId
        project
            |> buildProject
            |> ifValid \case
                Left project -> render EditView { .. }
                Right project -> do
                    project <- project |> updateRecord
                    setSuccessMessage "Project updated"
                    redirectTo EditProjectAction { .. }

    action CreateProjectAction = do
        let project = newRecord @Project
        project
            |> buildProject
            |> ifValid \case
                Left project -> render NewView { .. } 
                Right project -> do
                    project <- project |> createRecord
                    setSuccessMessage "Project created"
                    redirectTo ProjectsAction

    action DeleteProjectAction { projectId } = do
        project <- fetch projectId
        deleteRecord project
        setSuccessMessage "Project deleted"
        redirectTo ProjectsAction

buildProject project = project
    |> fill @'[]


printOnes = do
   putStrLn "1"
   printOnes