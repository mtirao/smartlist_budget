//
//  BudgetDTO.swift
//
//
//  Created by Marcos Tirao on 23/07/2024.
//

import Foundation
import Fluent
import Vapor

struct BudgetDTO: Content {
    let id: UUID?
    let name: String?
    let amount: Double?
    let date: Date?
    
    init(id: UUID? = nil, name: String? = nil, amount: Double? = nil, date: Date? = nil) {
        self.id = id
        self.name = name
        self.amount = amount
        self.date = date
    }
    
    func toModel(userId: String) -> Budget {
        let model = Budget()
        
        model.id = self.id
        if let name = self.name {
            model.name = name
        }
        
        if let date {
            model.date = date
        }else {
            model.date = Date()
        }
        
        if let amount = self.amount {
            model.amount = amount
        }
        
        model.userId = userId
        
        return model
    }
}

/// Budget result

struct BudgetsDTO: Content {
    let budgets: [BudgetDescriptionDTO]
    
    init(budgets: [Date : [BillDTO]]) {
        guard budgets.isEmpty == false else {
            self.budgets = []
            return
        }
        var aux: [BudgetDescriptionDTO] = []
        
        for date in budgets.keys {
            aux.append( BudgetDescriptionDTO(date: date.description, bills: budgets[date] ?? []) )
        }
        self.budgets = aux
    }
}

struct BudgetDescriptionDTO: Content {
    let date: String
    let bills: [BillDTO]
}

struct BillDTO: Content {
    let name: String
    let amount: Double
}
