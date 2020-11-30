package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

import ci.gouv.dgbf.system.actor.server.persistence.api.IdentificationFormPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationForm;

@ApplicationScoped
public class IdentificationFormPersistenceImpl extends AbstractPersistenceEntityImpl<IdentificationForm> implements IdentificationFormPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	protected void __addReadQueryCollectInstances__() {
		
	}
}