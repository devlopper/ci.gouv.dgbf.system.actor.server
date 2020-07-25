package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.BudgetaryFunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetaryFunction;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class BudgetaryFunctionPersistenceImpl extends AbstractPersistenceEntityImpl<BudgetaryFunction> implements BudgetaryFunctionPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}