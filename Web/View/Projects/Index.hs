module Web.View.Projects.Index where
import Web.View.Prelude

data IndexView = IndexView { projects :: [Project] }

instance View IndexView ViewContext where
    html IndexView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={ProjectsAction}>Projects</a></li>
            </ol>
        </nav>
        <h1>Index <a href={pathTo NewProjectAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Project</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach projects renderProject}</tbody>
            </table>
        </div>
    |]


renderProject project = [hsx|
    <tr>
        <td>{project}</td>
        <td><a href={ShowProjectAction (get #id project)}>Show</a></td>
        <td><a href={EditProjectAction (get #id project)} class="text-muted">Edit</a></td>
        <td><a href={DeleteProjectAction (get #id project)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
