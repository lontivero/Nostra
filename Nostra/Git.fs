namespace Nostra

open System

type RepoId = RepoId of string

type RepositoryAnnouncement =
    { Id : RepoId
      Name : string option
      Description : string option
      Web : Uri option
      Clone : Uri option
      Relays : Uri list
    }
    with static member Default = {
            Id = RepoId (Guid.NewGuid().ToString()); Name = None; Description = None; Web = None; Clone = None; Relays = [] }

type GitRepoReference =
    { RepoId : RepoId
      RepoOwnerId : AuthorId
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

module Git =
    module GitRepoReference =
        let createTags { RepoId = repoId; RepoOwnerId = repoOwnerId } =
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


    let announceRepository announcement =
        let tags = [
            Some (let (RepoId id) = announcement.Id in Tag.createSingle "d" id)
            Option.map (Tag.createSingle "name") announcement.Name
            Option.map (Tag.createSingle "description") announcement.Description
            Option.map (Tag.createSingle "web") (announcement.Web |> Option.map _.ToString())
            Option.map (Tag.createSingle "clone") (announcement.Clone |> Option.map _.ToString())
            match announcement.Relays with
            | [] -> None
            | relays -> Some (Tag.create "relays" (relays |> List.map _.ToString()) )
        ]
        Event.create Kind.GitRepositoryAnnouncement (tags |> List.choose id) ""

    let patch (patchSubmit : GitPatchSubmit) =
        let (RepoId repoId) = patchSubmit.RepoRef.RepoId
        let tags =
            GitRepoReference.createTags patchSubmit.RepoRef
            @ (patchSubmit.CommitInfo |> Option.map GitPatchCommitInfo.createTags |> Option.defaultValue [])
            @ [ Tag.createSingle "t" $"A git patch for {repoId}" ]
        Event.create Kind.GitPatch tags patchSubmit.Patch

    let issue repoRef content =
        let tags = GitRepoReference.createTags repoRef
        Event.create Kind.GitIssue tags content
