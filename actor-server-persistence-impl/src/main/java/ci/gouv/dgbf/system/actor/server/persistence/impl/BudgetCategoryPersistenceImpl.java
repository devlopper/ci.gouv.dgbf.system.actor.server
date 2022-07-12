package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.BudgetCategoryPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetCategory;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class BudgetCategoryPersistenceImpl extends AbstractPersistenceEntityImpl<BudgetCategory> implements BudgetCategoryPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}