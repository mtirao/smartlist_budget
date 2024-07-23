//
//  UpdateBudget
//
//
//  Created by Marcos Tirao on 25/07/2024.
//

import Foundation
import Fluent

struct UpdateBudgetToSchema: Migration {
    func prepare(on database: Database) -> EventLoopFuture<Void> {
        database.schema("budget")
            .field("user_id", .string, .required, .sql(.default("")))
            .update()
    }
    
    func revert(on database: Database)  -> EventLoopFuture<Void> {
        database.schema("budget")
            .deleteField("user_id")
            .update()
    }
}
