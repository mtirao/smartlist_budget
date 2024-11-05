import Fluent
import Vapor

let apiVersion: PathComponent = "v1"

func routes(_ app: Application) throws {
    app.get { req async in
        "It works!"
    }

    app.get("hello") { req async -> String in
        "Hello, world!"
    }
    
    try app.register(collection: BudgetController())
    try app.register(collection: TenderController())
    try app.register(collection: InvoiceController())
}
