namespace Nostra

open System

type RepoId = RepoId of string

type GitRepoReference =
    { RepoId : RepoId
      RepoOwnerId : AuthorId
    }

type RepositoryAnnouncement =
    { Id : RepoId
      Name : string option
      Description : string option
      Web : Uri option
      Clone : Uri list
      Relays : Uri list
      Maintainers : AuthorId list
      EarliestUniqueCommit : string option
    }
    with static member Default = {
            Id = RepoId (Guid.NewGuid().ToString()); Name = None; Description = None; Web = None; Clone = []; Relays = []; Maintainers = []; EarliestUniqueCommit = None }

type GitRef =
    { Name : string
      CommitId : string
    }

type RepositoryState =
    { RepoRef : GitRepoReference
      Head : string option
      Branches : GitRef list
      Tags : GitRef list
    }

type GitPatchCommitterInfo =
    { Name : string
      Email : string
      Timestamp : string
      TimeZone : string
    }

type GitPatchCommitInfo =
    { Commit : string
      ParentCommit : string
      PgpSignature : string
      Committer : GitPatchCommitterInfo option
    }

type GitPatchSubmit =
    { RepoRef : GitRepoReference
      Patch : string
      CommitInfo : GitPatchCommitInfo option
    }

type GitPullRequest =
    { RepoRef : GitRepoReference
      Subject : string
      Labels : string list
      TipCommit : string
      CloneUrl : Uri
      Branch : string
      MergeBase : string option
    }

type GitPullRequestUpdate =
    { OriginalPrEventId : EventId
      OriginalPrAuthor : AuthorId
      TipCommit : string
      CloneUrl : Uri
      Branch : string
    }

type GitIssue =
    { RepoRef : GitRepoReference
      Subject : string option
      Labels : string list
      Content : string
    }

type GitStatusKind =
    | Open
    | Applied of commitId: string option
    | Closed
    | Draft

type GitStatus =
    { TargetEventId : EventId
      TargetAuthor : AuthorId
      RepoRef : GitRepoReference option
      Status : GitStatusKind
    }

module Git =
    module GitRepoReference =
        let createTags { RepoId = RepoId repoId; RepoOwnerId = repoOwnerId } =
            [
            Tag.create "a" [$"30617:{(repoOwnerId |> AuthorId.toHex)}:{repoId}"]
            Tag.authorTag repoOwnerId
            ]

    module GitPatchCommitterInfo =
        let createTag committerInfo =
            Tag.create "committer" [
                committerInfo.Name
                committerInfo.Email
                committerInfo.Timestamp
                committerInfo.TimeZone
            ]

    module GitPatchCommitInfo =
        let createTags commitInfo =
            [
            Some (Tag.createSingle "commit" commitInfo.Commit)
            Some (Tag.createSingle "parent-commit" commitInfo.ParentCommit)
            Some (Tag.createSingle "commit-pgp-sig" commitInfo.PgpSignature)
            Option.map GitPatchCommitterInfo.createTag commitInfo.Committer
            ] |> List.choose id


    let announceRepository (announcement : RepositoryAnnouncement) =
        let (RepoId repoId) = announcement.Id
        let tags = [
            Some (Tag.createSingle "d" repoId)
            Option.map (Tag.createSingle "name") announcement.Name
            Option.map (Tag.createSingle "description") announcement.Description
            Option.map (fun (u: Uri) -> Tag.createSingle "web" (u.ToString())) announcement.Web
            match announcement.Relays with
            | [] -> None
            | relays -> Some (Tag.create "relays" (relays |> List.map _.ToString()))
            Option.map (fun c -> Tag.create "r" [c; "euc"]) announcement.EarliestUniqueCommit
        ]
        let cloneTags = announcement.Clone |> List.map (fun u -> Tag.createSingle "clone" (u.ToString()))
        let maintainerTags = announcement.Maintainers |> List.map Tag.authorTag
        Event.create Kind.GitRepositoryAnnouncement ((tags |> List.choose id) @ cloneTags @ maintainerTags) ""

    let repositoryState (state: RepositoryState) =
        let (RepoId repoId) = state.RepoRef.RepoId
        let optionalTags = [
            Some (Tag.createSingle "d" repoId)
            Option.map (Tag.createSingle "HEAD") state.Head
        ]
        let branchTags = state.Branches |> List.map (fun (r: GitRef) -> Tag.create "refs/heads" [r.Name; r.CommitId])
        let tagTags = state.Tags |> List.map (fun (r: GitRef) -> Tag.create "refs/tags" [r.Name; r.CommitId])
        Event.create Kind.GitRepositoryState ((optionalTags |> List.choose id) @ GitRepoReference.createTags state.RepoRef @ branchTags @ tagTags) ""

    let patch (patchSubmit : GitPatchSubmit) =
        let (RepoId repoId) = patchSubmit.RepoRef.RepoId
        let tags =
            GitRepoReference.createTags patchSubmit.RepoRef
            @ (patchSubmit.CommitInfo |> Option.map GitPatchCommitInfo.createTags |> Option.defaultValue [])
            @ [ Tag.createSingle "t" $"A git patch for {repoId}" ]
        Event.create Kind.GitPatch tags patchSubmit.Patch

    let pullRequest (pr: GitPullRequest) =
        let tags =
            GitRepoReference.createTags pr.RepoRef
            @ [ Tag.createSingle "subject" pr.Subject ]
            @ (pr.Labels |> List.map (Tag.createSingle "t"))
            @ [ Tag.createSingle "c" pr.TipCommit
                Tag.createSingle "clone" (pr.CloneUrl.ToString())
                Tag.createSingle "branch" pr.Branch ]
            @ (pr.MergeBase |> Option.map (Tag.createSingle "merge-base") |> Option.toList)
        Event.create Kind.GitPullRequest tags ""

    let pullRequestUpdate update =
        let tags = [
            Tag.create "E" [update.OriginalPrEventId |> EventId.toHex]
            Tag.create "P" [update.OriginalPrAuthor |> AuthorId.toHex]
            Tag.createSingle "c" update.TipCommit
            Tag.createSingle "clone" (update.CloneUrl.ToString())
            Tag.createSingle "branch" update.Branch
        ]
        Event.create Kind.GitPullRequestUpdate tags ""

    let issue (gitIssue: GitIssue) =
        let tags =
            GitRepoReference.createTags gitIssue.RepoRef
            @ (gitIssue.Subject |> Option.map (Tag.createSingle "subject") |> Option.toList)
            @ (gitIssue.Labels |> List.map (Tag.createSingle "t"))
        Event.create Kind.GitIssue tags gitIssue.Content

    let issueReply replyToIssue replyToAuthor rootIssue content =
        let tags = [
            Tag.create "e" [replyToIssue |> EventId.toHex; ""; "reply"]
            Tag.create "e" [rootIssue |> EventId.toHex; ""; "root"]
            Tag.authorTag replyToAuthor
        ]
        Event.create Kind.GitIssueReply tags content

    let status (gitStatus: GitStatus) =
        let kind, extraTags =
            match gitStatus.Status with
            | Open -> Kind.GitStatusOpen, []
            | Applied commitId ->
                Kind.GitStatusApplied,
                commitId |> Option.map (fun c -> Tag.createSingle "applied-commit" c) |> Option.toList
            | Closed -> Kind.GitStatusClosed, []
            | Draft -> Kind.GitStatusDraft, []

        let tags =
            [ Tag.create "e" [gitStatus.TargetEventId |> EventId.toHex]
              Tag.authorTag gitStatus.TargetAuthor ]
            @ (gitStatus.RepoRef |> Option.map GitRepoReference.createTags |> Option.defaultValue [])
            @ extraTags
        Event.create kind tags ""
