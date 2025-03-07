
# hec-applicant-frontend
More information about the HEC (Hidden Economy Conditionality) project can be found [here](https://www.gov.uk/government/publications/new-tax-checks-on-licence-renewal-applications).

This microservice serves the public digital UI to allow licence applicants to perform a tax check. If 
successful, applicants will receive a tax check code at the end of the journey that they can give 
to the relevant licencing bodies (LB's) to allow their licence to be issued.

## Running the service

### Using service manager

When running locally, the dependant services can be run using the service manager command
```
sm2 --start HEC_DEP
```
All HEC services can run via
```
sm2 --start HEC_ALL
```
By default, this service runs on port `10106`.

To stop the frontend microservice from running on service manager (e.g. to run your own version locally), you can run:

```
sm2 -stop HEC_APPLICANT_FRONTEND
```


### Using localhost

To run this frontend microservice locally on the configured port **'9222'**, you can run:

```
sbt run 
```

**NOTE:** Ensure that you are not running the microservice via service manager before starting your service locally (vice versa)
or the service will fail to start

## Accessing the service

This service requires authentication stubbing before it can be accessed. Details can be found on the
[DDCY Live Services Credentials sheet](https://docs.google.com/spreadsheets/d/1ecLTROmzZtv97jxM-5LgoujinGxmDoAuZauu2tFoAVU/edit?gid=1186990023#gid=1186990023)
for both staging and local url's or check the Tech Overview section in the
[service summary page](https://confluence.tools.tax.service.gov.uk/display/ELSY/HEC+Service+Summary)

Additional test data can be found within the [hec-stub](https://github.com/hmrc/hec-stubs) repository.
Check the README to see different data states.


## Running tests via terminal

You can run tests in Intellij by running:

```
sbt test
```


## Patterns 

### Starting a journey
A journey can be started via the start endpoint
```
GET /tax-check-for-licence/start
```
This will prompt the user to log in if there isn't an active session yet. 

Different journeys can be started via the test-only journey starter page at
```
GET /tax-check-for-licence/test-only/start-journeys
```
When a journey is selected and submitted here, the login session is set up in the background with the appropriate
test data, thereby bypassing the login step on the UI. To enable the test-only endpoint, start the service with 
the option
```
sbt run -Dplay.http.router=testOnlyDoNotUseInAppConf.Routes
```

### Navigation
The rules defining the route from one page to another are contained in `JourneyService`. As well as defining the routes
forward, this service also automatically calculates the previous page relative to a current page based upon the session 
state. This is used to make the back links on the pages point to the right place. 

A distinction is made between "incomplete" user answers and "complete" user answers to represent the answers a user has
given during their journey. This distinction is made to facilitate navigation with the "check your answers" pattern. The
`JourneyService` is responsible for uplifting "incomplete" answers to "complete" ones if all questions have been 
answered. 

### Scalafmt
This repository uses [Scalafmt](https://scalameta.org/scalafmt/), a code formatter for Scala. The formatting rules configured for this repository are defined within [.scalafmt.conf](.scalafmt.conf).

To apply formatting to this repository using the configured rules in [.scalafmt.conf](.scalafmt.conf) execute:

 ```
 sbt scalafmtAll
 ```

To check files have been formatted as expected execute:

 ```
 sbt scalafmtCheckAll scalafmtSbtCheck
 ```

[Visit the official Scalafmt documentation to view a complete list of tasks which can be run.](https://scalameta.org/scalafmt/docs/installation.html#task-keys)

## License

This code is open source software licensed under the [Apache 2.0 License]("http://www.apache.org/licenses/LICENSE-2.0.html").


## Other helpful documentation

* [Service Runbook](https://confluence.tools.tax.service.gov.uk/display/ELSY/Economic+Crime+Levy+%28ECL%29+Runbook)

* [Architecture Links](https://confluence.tools.tax.service.gov.uk/pages/viewpage.action?pageId=872972492)