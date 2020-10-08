package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.ExecutableImputationPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutableImputation;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class ExecutableImputationPersistenceImpl extends AbstractPersistenceEntityImpl<ExecutableImputation> implements ExecutableImputationPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}