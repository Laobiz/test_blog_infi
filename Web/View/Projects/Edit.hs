module Web.View.Projects.Edit where
import Web.View.Prelude

data EditView = EditView { project :: Project }

instance View EditView ViewContext where
    html EditView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={ProjectsAction}>Projects</a></li>
                <li class="breadcrumb-item active">Edit Project</li>
            </ol>
        </nav>
        <h1>Edit Project</h1>
        {renderForm project}
    |]

renderForm :: Project -> Html
renderForm project = formFor project [hsx|

    {submitButton}
|]
