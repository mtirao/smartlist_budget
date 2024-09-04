import Fluent
import Vapor

struct BudgetController: RouteCollection {
    
    let emptyBudgetDTO = BudgetsDTO(budgets: [:])
    
    func boot(routes: RoutesBuilder) throws {
        let budgets = routes.grouped("api", apiVersion, "budget")

        budgets.get(use: self.index)
        budgets.post(use: self.create)
        
        budgets.group(":budgetID") { todo in
            todo.delete(use: self.delete)
        }
        
        budgets.group(":userID") { todo in
            todo.get(use: self.fetch)
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
    
    @Sendable
    func fetch(req: Request) async throws -> BudgetsDTO {
        guard let userId = req.parameters.get("userID") else { return emptyBudgetDTO }
        
        let result = try await Budget.query(on: req.db).filter(\.$userId == userId).all().map { $0.toDTO() }.filter{ $0.date != nil}
        if result.isEmpty {
            return emptyBudgetDTO
        }
        
        var budgets: [Date : [BillDTO]] = [:]
        for budget in result {
            if budgets[budget.date!]?.isEmpty == true {
                budgets[budget.date!]? = [BillDTO(name: budget.name ?? "", amount: budget.amount ?? 0.0)]
            }else {
                budgets[budget.date!]?.append(BillDTO(name: budget.name ?? "", amount: budget.amount ?? 0.0))
            }
        }
        
        return BudgetsDTO(budgets: budgets)
    }
}
