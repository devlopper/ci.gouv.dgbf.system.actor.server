package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.RequestPrivilegeBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.RequestPrivilegePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestPrivilege;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class RequestPrivilegeBusinessImpl extends AbstractBusinessEntityImpl<RequestPrivilege, RequestPrivilegePersistence> implements RequestPrivilegeBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
