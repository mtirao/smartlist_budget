//
//  BudgetView.swift
//  smartlist_budget
//
//  Created by Marcos Tirao on 11/11/2024.
//

import Fluent
import Foundation

final class BudgetView: Model, @unchecked Sendable {
    static let schema = "budgets_view"
    
    @ID(custom: "user_id")
    var id: String?
    
    @Field(key: "amount")
    var amount: Double
    
    @Field(key: "date")
    var date: Date
    
    @Field(key: "name")
    var name: String
    
    init() { }
    
    init(id: String? = nil, amount: Double, date: Date, name: String) {
        self.id = id
        self.amount = amount
        self.date = date
        self.name = name
    }
    
}
