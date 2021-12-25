package ci.gouv.dgbf.system.actor.server.business.api;

import javax.transaction.Transactional;

import org.cyk.utility.server.business.BusinessEntity;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Identity;

public interface IdentityBusiness extends BusinessEntity<Identity> {

	@Transactional
	Identity createFromInterface(Identity.Interface identity);
	
	String encryptElectroncicMailAddress(String electronicMailAddress);
	
	String decryptElectroncicMailAddress(String encryptElectronicMailAddress);
}