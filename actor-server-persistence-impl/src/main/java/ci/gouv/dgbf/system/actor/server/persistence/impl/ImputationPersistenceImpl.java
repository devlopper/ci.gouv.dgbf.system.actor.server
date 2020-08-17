package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.ImputationPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Imputation;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class ImputationPersistenceImpl extends AbstractPersistenceEntityImpl<Imputation> implements ImputationPersistence,Serializable {
	private static final long serialVersionUID = 1L;
	
}