import Fluent
import FluentSQL
import Vapor

struct BudgetController: RouteCollection {
    
    let emptyBudgetDTO = BudgetsDTO(budgets: [:])
    
    func boot(routes: RoutesBuilder) throws {
        let budgets = routes.grouped("api", apiVersion, "budget")
        budgets.post(use: self.create)
        budgets.get(use: self.fetch)
    }

    @Sendable
    func create(req: Request) async throws -> BudgetDTO {
        guard let userId = req.parameters.get("userID") else { return BudgetDTO() }
        
        let todo = try req.content.decode(BudgetDTO.self).toModel(userId: userId)
        try await todo.save(on: req.db)
        return todo.toDTO()
    }
    
    @Sendable
    func fetch(req: Request) async throws -> BudgetsDTO {
        guard let userId = req.parameters.get("userID") else { return emptyBudgetDTO }
        guard let sql = req.db as? SQLDatabase else { return emptyBudgetDTO }
        
        let budgets = try await sql.raw(
            "select user_id,name, date, sum(amount) as amount from budgets where user_id = '\(unsafeRaw: userId)' group by user_id,date,name")
            .all(decodingFluent: Budget.self)
            .map { $0.toDTO() }
            .filter{ $0.date != nil}
            
        if !budgets.isEmpty {
           let result =  budgets
                    .grouped(by: \.date)
                    .compactMapValues{ $0.map{ BillDTO(name: $0.name ?? "Other", amount: $0.amount ?? 0)} }
                    
            return BudgetsDTO(budgets: result)
        }
        
        return emptyBudgetDTO
    }
}
