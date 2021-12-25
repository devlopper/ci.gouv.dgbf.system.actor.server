package ci.gouv.dgbf.system.actor.server.business.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import javax.inject.Inject;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.business.api.IdentityBusiness;
import ci.gouv.dgbf.system.actor.server.business.impl.integration.ApplicationScopeLifeCycleListener;

public class IdentityBusinessImplUnitTest extends AbstractUnitTestMemory {
	private static final long serialVersionUID = 1L;

	@Inject
	private IdentityBusiness identityBusiness;
	
	@BeforeAll
	public static void beforeAll() {
		ApplicationScopeLifeCycleListener.INTEGRATION = Boolean.FALSE;
	}
	
	@Override
	protected String getPersistenceUnitName() {
		return "requests";
	}
	
	@Test
	public void encryptAndEncodeElectroncicMailAddress() {
		assertThat(identityBusiness.encryptElectroncicMailAddress("kycdev@gmail.com")).isEqualTo("rb0T00EKk2Fxuviwxu1LXuMX2j4yo+KFt0pQI14PMJo=");
	}
	
	@Test
	public void decodeAndDecryptElectroncicMailAddress() {
		assertThat(identityBusiness.decryptElectroncicMailAddress("rb0T00EKk2Fxuviwxu1LXuMX2j4yo+KFt0pQI14PMJo=")).isEqualTo("kycdev@gmail.com");
	}
}