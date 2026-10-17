{:user "your-github-user"
 :model "qwen3:4b"
 ;; Claude model over ACP (Claude Code alias: haiku, sonnet, opus, ...).
 ;; Overridden with --model. nil = whatever Claude Code has configured.
 :claude-model "haiku"
 ;; Command prefix (the path is appended) to open a terminal in a directory (:terminal),
 ;; and to run a script in a new terminal (:claude). macOS:
 :terminal "open -a Terminal "
 :terminal-run "open -a Terminal "
 ;; Linux, e.g. kitty:  :terminal "kitty --directory "   :terminal-run "kitty "
 ;;        alacritty:   :terminal "alacritty --working-directory "   :terminal-run "alacritty -e "
 ;; Branch that PRs are compared against (branch-diff)
 :base-branch "develop"
 :ollama "http://localhost:11434/api/chat"
 ;; Jira: base URL (the JIRA_URL env var overrides it) and project key of the tickets
 :jira-url "https://your-org.atlassian.net"
 :jira-project "ABC"
 ;; Docker services (compose service name, in any project) that have to be up
 ;; for development; `start` says how to bring up the ones not in any project
 :docker-services [:postgres :redis :nginx]
 :docker-start "cd ~/Development/your-dev-env && ./run start-background"
 ;; List of repository names (eg :backend)
 :repo-names [:backend :frontend :ml :docs]
 ;; Table of repo / url (eg :backend "github-user/backend"
 :repos {:backend "your-org/backend"
         :frontend "your-org/frontend"
         :ml "your-org/ml"
         :docs "your-org/docs"}
 ;; Table of repo / local dir (eg :backend "~/dev/bend"
 :local-repos (let [dev (.. (os.getenv "HOME") "/Development/")]
                {:backend (.. dev "backend")
                 :frontend (.. dev "frontend")
                 :ml (.. dev "ml")
                 :docs (.. dev "docs")})}
