import NIOSSL
import Fluent
import FluentPostgresDriver
import Vapor
import JWT

// configures your application
public func configure(_ app: Application) async throws {
    // uncomment to serve files from /Public folder

    // Add HMAC with SHA-256 signer.
    await app.jwt.keys.add(hmac: "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9", digestAlgorithm: .sha256)
    // app.middleware.use(FileMiddleware(publicDirectory: app.directory.publicDirectory))

    app.databases.use(DatabaseConfigurationFactory.postgres(configuration: .init(
        hostname: Environment.get("DATABASE_HOST") ?? "localhost",
        port: Environment.get("DATABASE_PORT").flatMap(Int.init(_:)) ?? SQLPostgresConfiguration.ianaPortNumber,
        username: Environment.get("DATABASE_USERNAME") ?? "mtirao",
        password: Environment.get("DATABASE_PASSWORD") ?? "",
        database: Environment.get("DATABASE_NAME") ?? "smartlist_budget",
        tls: .prefer(try .init(configuration: .clientDefault)))
    ), as: .psql)

    app.middleware.use(AuthenticateMiddleware())
    
    app.migrations.add(CreateBudget())
    app.migrations.add(CreateInvoice())
    app.migrations.add(CreateTender())
    app.migrations.add(CreateItem())
    app.migrations.add(CreateBasket())
    app.migrations.add(CreateBasketDescription())
    // register routes
    try routes(app)
}
