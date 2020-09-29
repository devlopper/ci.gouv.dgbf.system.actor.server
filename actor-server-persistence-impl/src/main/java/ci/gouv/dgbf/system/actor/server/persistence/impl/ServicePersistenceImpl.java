package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.ServicePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Service;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class ServicePersistenceImpl extends AbstractPersistenceEntityImpl<Service> implements ServicePersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}