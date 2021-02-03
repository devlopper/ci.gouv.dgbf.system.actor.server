package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.ExpenditureNaturePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExpenditureNature;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class ExpenditureNaturePersistenceImpl extends AbstractPersistenceEntityImpl<ExpenditureNature> implements ExpenditureNaturePersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}