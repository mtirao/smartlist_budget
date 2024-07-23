import Fluent
import Vapor

struct BudgetController: RouteCollection {
    func boot(routes: RoutesBuilder) throws {
        let budgets = routes.grouped("api", apiVersion, "budget")

        budgets.get(use: self.index)
        budgets.post(use: self.create)
        budgets.group(":budgetID") { todo in
            budgets.delete(use: self.delete)
        }
    }

    @Sendable
    func index(req: Request) async throws -> [BudgetDTO] {
        return try await Budget.query(on: req.db).all().map { $0.toDTO() }
    }

    @Sendable
    func create(req: Request) async throws -> BudgetDTO {
        let todo = try req.content.decode(BudgetDTO.self).toModel()

        try await todo.save(on: req.db)
        return todo.toDTO()
    }

    @Sendable
    func delete(req: Request) async throws -> HTTPStatus {
        guard let budget = try await Budget.find(req.parameters.get("budgetID"), on: req.db) else {
            throw Abort(.notFound)
        }

        try await budget.delete(on: req.db)
        return .noContent
    }
}
