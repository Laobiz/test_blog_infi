module Web.View.Projects.New where
import Web.View.Prelude

data NewView = NewView { project :: Project }

instance View NewView ViewContext where
    html NewView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={ProjectsAction}>Projects</a></li>
                <li class="breadcrumb-item active">New Project</li>
            </ol>
        </nav>
        <h1>New Project</h1>
        {renderForm project}
    |]

renderForm :: Project -> Html
renderForm project = formFor project [hsx|

    {submitButton}
|]
