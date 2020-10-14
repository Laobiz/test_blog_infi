module Web.View.Projects.Show where
import Web.View.Prelude

data ShowView = ShowView { project :: Project }

instance View ShowView ViewContext where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={ProjectsAction}>Projects</a></li>
                <li class="breadcrumb-item active">Show Project</li>
            </ol>
        </nav>
        <h1>Show Project</h1>
    |]
