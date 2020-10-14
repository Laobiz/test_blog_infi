module Web.Types where
import IHP.Prelude
import qualified IHP.Controller.Session
import qualified IHP.ControllerSupport as ControllerSupport
import IHP.ModelSupport
import Application.Helper.Controller
import IHP.ViewSupport
import Generated.Types

data WebApplication = WebApplication deriving (Eq, Show)

data ViewContext = ViewContext
    { requestContext :: ControllerSupport.RequestContext
    , flashMessages :: [IHP.Controller.Session.FlashMessage]
    , controllerContext :: ControllerSupport.ControllerContext
    , layout :: Layout
    }

data ProjectsController
    = ProjectsAction
    | NewProjectAction
    | ShowProjectAction { projectId :: !(Id Project) }
    | CreateProjectAction
    | EditProjectAction { projectId :: !(Id Project) }
    | UpdateProjectAction { projectId :: !(Id Project) }
    | DeleteProjectAction { projectId :: !(Id Project) }
    deriving (Eq, Show, Data)
