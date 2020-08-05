package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.BudgetSpecializationUnitPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetSpecializationUnit;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class BudgetSpecializationUnitPersistenceImpl extends AbstractPersistenceEntityImpl<BudgetSpecializationUnit> implements BudgetSpecializationUnitPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}