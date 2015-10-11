## Purpose of this experiment

I very often come across ideas for (web-based) products that I want to throw
together a proof of concept for.

We all know that Haskell productivity will easily win (over most things)
hands-down with any established codebase (has seen more than ~3 months of work).

However, when starting those new project ideas, getting off the ground and
having something to play around with in a day or at most two is sometimes the
difference between trying the idea out and not trying it out. So far, Rails has
been the best way to do these types of things, as it will definitely let you get
off the ground within a day.

The question I pose with this repo is:
Is it possible to achieve the same level of off-the-ground productivity in Haskell?

## The goal

What we need to end up with is a template application that has all the
basics in place for what you need for a productive starting environment.

Some of those things are (I'll try to list more comprehensively later):

* Routing
* Database handling (connecting, environment handling, migrations)
* Efficient modeling + ORM
* User and session handling (all apps need login sooner or later)
* Clear and separated controller logic
* Clear and separated views
* A minimum of boilerplate to get in your way
* A way to generate a scaffold (Model, View and Controller)

## Building

This project is meant to work with `stack`. `stack build` should be
enough to install GHC and all dependencies. The goal is to be newbie
friendly so if we ever need something other than `stack build` there's
something wrong.

## Running

To be able to run this application you need to have PostgreSQL
installed. Please check out their instructions to get it up and running
if you haven't already.

I have made the `bin/` folder to make it super easy to get started.

`bin/create_dbs` should call out to PostgreSQL to create all the
databases needed.

`bin/serve` should start up a `halive` instance which will automatically
recompile the code on any changes so you can just save the code and then reload
the browser so see your changes (any more work in the way of developing is
insane). It could be possible to hook up `halive` to something like
[BrowserSync](http://www.browsersync.io/) to make the environment even
better but that is low-priority.

So basically all that's needed should be

* Install postgres
* Install [stack](https://github.com/commercialhaskell/stack)
* `stack build`
* `bin/create_dbs`
* `bin/serve`

## TODO

This is very much a work in progress and I may abandon the project
before I reach the finish. But here are some basic things that still
need to be done.

* [ ] Overall refactor and make the code organization make sense
* [ ] Make the views properly hierarchical (layout -> view -> partials)
* [ ] Reduce boilerplate; doing the execessive module re-exporting I do
  just to not have to import a bunch of things everywhere makes it
  really difficult to add new things.
* [ ] `Config.hs` is way overloaded, i've thrown a bit of everything in
  there
