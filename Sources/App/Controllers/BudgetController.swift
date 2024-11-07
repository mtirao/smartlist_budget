import Fluent
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
        
        let result = try await Budget.query(on: req.db).filter(\.$userId == userId).all().map { $0.toDTO() }.filter{ $0.date != nil}.grouped(by: \.date)
        if result.isEmpty {
            return emptyBudgetDTO
        }
        
        var budgets: [Date : [BillDTO]] = [:]
        
        for (date, value) in result.mapValues({$0.grouped(by: \.name)}) {
            var bills: [BillDTO] = []
            for (name, value1) in value {
                let amount = value1.reduce(0, {accum, newValue in (newValue.amount ?? 0) + accum})
                bills.append(BillDTO(name: name ?? "Other", amount: amount))
            }
            if let date {
                budgets[date] = bills
            }
        }
        
        return BudgetsDTO(budgets: budgets)
    }
}
