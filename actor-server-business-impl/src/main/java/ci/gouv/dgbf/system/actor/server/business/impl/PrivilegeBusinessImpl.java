package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;
import javax.transaction.Transactional;

import org.cyk.utility.persistence.EntityManagerGetter;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.PrivilegeBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.PrivilegePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.PrivilegeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Privilege;

@ApplicationScoped
public class PrivilegeBusinessImpl extends AbstractBusinessEntityImpl<Privilege, PrivilegePersistence> implements PrivilegeBusiness,Serializable {
	private static final long serialVersionUID = 1L;

	@Override @Transactional
	public void refresh() {
		PrivilegeQuerier.getInstance().refresh(EntityManagerGetter.getInstance().get());
	}		
}