package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.ServiceBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ServicePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Service;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class ServiceBusinessImpl extends AbstractBusinessEntityImpl<Service, ServicePersistence> implements ServiceBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
